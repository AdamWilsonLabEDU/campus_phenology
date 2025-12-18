# -----------------------------------------------------------
# Libraries
# -----------------------------------------------------------
library(MODISTools)
library(dplyr)
library(lubridate)
library(arrow)
library(piggyback)
library(fs)
library(glue)

# -----------------------------------------------------------
# Parameters
# -----------------------------------------------------------
lat <- 42.996
lon <- -78.787
pixel_radius_km <- 0.5
product <- "MOD13Q1"
band <- "250m_16_days_NDVI"
quality_filter <- TRUE
local_dir <- "data/modis_semesters"
dir_create(local_dir)

repo <- "AdamWilsonLabEDU/campus_phenology"

# -----------------------------------------------------------
# Helper: define semester start/end dates
# -----------------------------------------------------------
get_semester_dates <- function(year) {
  list(
    Spring = list(start = as.Date(glue("{year}-01-01")), end = as.Date(glue("{year}-05-31"))),
    Fall   = list(start = as.Date(glue("{year}-09-01")), end = as.Date(glue("{year}-12-31")))
  )
}

# Generate a list of semesters from 2000 to current year
years <- 2000:year(Sys.Date())
semester_list <- lapply(years, get_semester_dates)
semester_list <- unlist(semester_list, recursive = FALSE)

# Determine current semester
current_date <- Sys.Date()
current_semester_name <- if (month(current_date) %in% 1:5) "Spring" else "Fall"
current_year <- year(current_date)
current_semester_tag <- glue("{current_semester_name}_{current_year}")

# -----------------------------------------------------------
# Function: download MODIS NDVI for a period
# -----------------------------------------------------------
download_modis_ndvi <- function(start_date, end_date) {
  ndvi_raw <- mt_subset(
    product = product,
    lat = lat,
    lon = lon,
    band = band,
    start = start_date,
    end = end_date,
    km_lr = pixel_radius_km,
    km_ab = pixel_radius_km,
    site_name = paste0("UB_MODIS_", start_date, "_", end_date),
    internal = TRUE,
    progress = TRUE
  )

  if (nrow(ndvi_raw) == 0) return(NULL)

  ndvi_clean <- ndvi_raw %>%
    mutate(date = ymd(paste0(year, "-", doy)),
           ndvi = value / 10000) %>%
    select(date, ndvi, quality, lon, lat)

  if (quality_filter) {
    ndvi_clean <- ndvi_clean %>% filter(quality %in% c(0,1))
  }

  ndvi_agg <- ndvi_clean %>%
    group_by(date) %>%
    summarize(
      ndvi_mean = mean(ndvi, na.rm = TRUE),
      ndvi_sd = sd(ndvi, na.rm = TRUE),
      n_pixels = n(),
      .groups = "drop"
    ) %>%
    arrange(date)

  return(ndvi_agg)
}

# -----------------------------------------------------------
# Function: download/update a single semester
# -----------------------------------------------------------
process_semester <- function(name, start_date, end_date) {
  file_path <- file.path(local_dir, glue("modis_ndvi_{name}.parquet"))

  # Only download if file doesn't exist OR if current semester
  if (file_exists(file_path) && name != current_semester_tag) {
    message("Skipping historical semester: ", name)
    return(NULL)
  }

  # For current semester, download starting from last saved date
  if (file_exists(file_path) && name == current_semester_tag) {
    existing <- arrow::read_parquet(file_path)
    last_date <- max(existing$date)
    if (last_date >= end_date) {
      message("Current semester already up to date: ", name)
      return(NULL)
    }
    start_date <- last_date + 1
  }

  # Download
  ndvi_sem <- download_modis_ndvi(start_date, end_date)
  if (is.null(ndvi_sem) || nrow(ndvi_sem) == 0) {
    message("No NDVI data for semester: ", name)
    return(NULL)
  }

  # Merge with existing data for current semester
  if (file_exists(file_path) && name == current_semester_tag) {
    existing <- arrow::read_parquet(file_path)
    ndvi_sem <- bind_rows(existing, ndvi_sem) %>%
      distinct(date, .keep_all = TRUE) %>%
      arrange(date)
  }

  # Save Parquet
  arrow::write_parquet(ndvi_sem, file_path)
  message("Saved semester NDVI: ", name)

  # Push to GitHub release
  existing_tags <- piggyback::pb_list(repo = repo)$tag
  if (!name %in% existing_tags) {
    piggyback::pb_new_release(repo = repo, tag = name, name = name)
  }
  piggyback::pb_upload(file_path, repo = repo, tag = name)
  message("Uploaded semester NDVI to GitHub: ", name)
}

# -----------------------------------------------------------
# Main Loop: process all semesters
# -----------------------------------------------------------
for (yr in 2000:year(Sys.Date())) {
  semesters <- get_semester_dates(yr)
  for (sem in names(semesters)) {
    sem_name <- glue("{sem}_{yr}")
    sem_start <- semesters[[sem]]$start
    sem_end   <- semesters[[sem]]$end

    # Only download past semesters if missing, always update current semester
    process_semester(sem_name, sem_start, sem_end)
  }
}
