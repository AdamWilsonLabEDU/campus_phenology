###############################################################################
# NPN STATUS DATA PIPELINE WITH GITHUB RELEASE (PIGGYBACK) BACKING STORE
#
# PURPOSE
#   - Incrementally download USA-NPN status data by semester
#   - Store semester-partitioned Parquet files in a GitHub Release via piggyback
#   - Avoid re-downloading data already available in the release
#   - Run identically on a laptop or in GitHub Actions (ephemeral filesystem)
#
# PREREQUISITES
#   - GitHub repository with an existing release tag (e.g., "npn-data")
#   - GITHUB_TOKEN available in environment (required in GitHub Actions)
#
# STORAGE MODEL
#   GitHub Release assets:
#     npn_obs_network-891_semester-YYYY.S.parquet
#
###############################################################################

# -------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------
library(tidyverse)
library(rnpn)
library(lubridate)
library(zoo)
library(arrow)
library(piggyback)
library(glue)


# -------------------------------------------------------------------
# User-configurable parameters
# -------------------------------------------------------------------

# Temporal scope of data to ensure availability
year_start <- 2019
year_stop  <- year(Sys.Date())

# USA-NPN network ID (UB network)
network_id <- 891

# GitHub repository and release tag used as object storage
repo        <- "AdamWilsonLabEDU/campus_phenology"
release_tag <- "npn-data"

# Observation requirements (used downstream in summaries)
required_weeks        <- 9
require_obs_per_week  <- 4
require_obs           <- required_weeks * require_obs_per_week

current_semester <- semester(Sys.Date(), with_year = TRUE)

# Local ephemeral cache (safe to delete at any time)
cache_dir <- "data/cache"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
base_dir  <- "data/processed"



# -------------------------------------------------------------------
# Ensure GitHub Release Exists (create if missing)
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Ensure GitHub Release Exists (piggyback-native)
# -------------------------------------------------------------------

release_exists <- TRUE

# pb_list() errors if the release does not exist; use that as the test
tryCatch(
  {
    piggyback::pb_list(
      tag  = release_tag,
      repo = repo
    )
  },
  error = function(e) {
    release_exists <<- FALSE
  }
)

# Create the release if it does not exist
if (!release_exists) {

  message("GitHub release '", release_tag, "' not found — creating it.")

  piggyback::pb_release_create(
    tag  = release_tag,
    repo = repo,
    name = release_tag,
    body = paste(
      "Automated data release for USA-NPN observations.",
      "Assets are semester-partitioned Parquet files.",
      "Current semester is refreshed on each run.",
      sep = "\n"
    )
  )
}


# -----------------------------
# Tree metadata
# -----------------------------
trees <- read_csv("data/trees.csv", show_col_types = FALSE) %>%
  mutate(
    color = case_match(
      common_name,
      "Red Oak"            ~ "red",
      "Black Oak"          ~ "black",
      "Silver Maple"       ~ "darkblue",
      "Red Maple"          ~ "orange",
      "Honey Locust"       ~ "purple",
      "Sugar Maple"        ~ "darkpurple",
      "Staghorn Sumac"     ~ "blue",
      "Eastern Cottonwood" ~ "beige",
      "Apple"              ~ "yellow",
      "American Basswood"  ~ "pink",
      "River Birch"        ~ "lightred",
      "White Birch"        ~ "cadetblue",
      "Ginkgo"             ~ "lightgray",
      .default             = NA_character_
    ),
    common_name_tag = paste(tag, common_name)
  ) %>%
  select(-OBJECTID)


# -------------------------------------------------------------------
# Determine which semesters *should* exist
#   Semester format matches lubridate::semester(with_year = TRUE)
#   Example: "2023.1", "2023.2"
# -------------------------------------------------------------------
expected_semesters <- expand_grid(
  year = year_start:year_stop,
  term = c(1, 2)
) %>%
  mutate(semester = paste(year, term, sep = ".")) %>%
  pull(semester)

closed_semesters <- setdiff(
  expected_semesters,
  current_semester
)

# -------------------------------------------------------------------
# Query GitHub Release to determine which semesters already exist
# -------------------------------------------------------------------
release_assets <- piggyback::pb_list(
  tag  = release_tag,
  repo = repo
)

available_semesters <- release_assets$file_name %>%
  str_extract("semester-[0-9]{4}\\.[12]") %>%
  str_remove("semester-") %>%
  na.omit()

# Always refresh current semester, even if it exists
missing_semesters <- union(
  setdiff(closed_semesters, available_semesters),
  current_semester
)

message(
  "Semesters missing from GitHub Release: ",
  ifelse(length(missing_semesters) == 0,
         "none",
         paste(missing_semesters, collapse = ", "))
)

# -------------------------------------------------------------------
# Download all existing Parquet assets from GitHub Release
#   (fast; avoids conditional logic for individual files)
# -------------------------------------------------------------------
if (length(release_assets) > 0) {
  piggyback::pb_download(
    tag  = release_tag,
    repo = repo,
    dest = cache_dir)
}

# -------------------------------------------------------------------
# Download only *missing* semesters from USA-NPN and upload them
# -------------------------------------------------------------------
if (length(missing_semesters) > 0) {

  # Determine which calendar years are required for missing semesters
  years_to_download <- unique(floor(as.numeric(missing_semesters)))

  # Download raw status observations
  raw_new <- npn_download_status_data(
    request_source    = "campus_phenology",
    years             = as.character(years_to_download),
    additional_fields = c(
      "Plant_Nickname",
      "ObservedBy_Person_ID",
      "Submission_Datetime"
    ),
    network_ids = network_id
  ) %>%
    as_tibble() %>%
    mutate(
      observation_date = ymd(observation_date),
      semester = semester(observation_date, with_year = TRUE)
    ) %>%
    filter(semester %in% missing_semesters)

  # Split by semester and persist each as its own Parquet asset
  split(raw_new, raw_new$semester) %>%
    walk(function(df) {

      sem <- unique(df$semester)

      out_file <- file.path(
        cache_dir,
        glue("npn_obs_network-{network_id}_semester-{sem}.parquet")
      )

      # Write Parquet locally
      write_parquet(df, out_file)

      # Upload Parquet to GitHub Release
      piggyback::pb_upload(
        file   = out_file,
        tag    = release_tag,
        repo   = repo,
        overwrite = TRUE
      )

    })
}

# -------------------------------------------------------------------
# Load all cached Parquet files into a single Arrow Dataset
# -------------------------------------------------------------------
obs_files <- list.files(
  cache_dir,
  pattern = "npn_obs_network-.*\\.parquet",
  full.names = TRUE
)

obs_ds <- open_dataset(obs_files)

# Materialize once for downstream processing
d <- obs_ds %>%
  collect() %>%
  mutate(
    # Core temporal fields
    submission_date   = as.Date(submission_datetime),
    observation_week  = isoweek(observation_date),
    observation_doy   = yday(observation_date),
    observation_year  = year(observation_date),

    # Re-map all observations to a common plotting year
    observation_datey = make_date(
      2020, month(observation_date), day(observation_date)
    ),

    # Clean sentinel missing values
#    abundance_value = na_if(abundance_value, -9999),
#    intensity_value = na_if(intensity_value, "-9999"),

    # Factor for grouping efficiency
    observedby_person_id = factor(observedby_person_id)) %>%
    rename(intensity_raw = intensity_value) %>%
      mutate(
        intensity = case_match(
          intensity_raw,
            "Less than 5%"        ~ 2.5,
            "Less than 25%"       ~ 20,
            "5-24%"               ~ 14.5,
            "25-49%"              ~ 37,
            "50-74%"              ~ 62,
           "75-94%"              ~ 84.5,
           "95% or more"         ~ 97.5,
            "Less than 3"         ~ 2,
          "3 to 10"             ~ 6.5,
          "11 to 100"           ~ 50,
          "101 to 1,000"        ~ 500,
          "1,001 to 10,000"     ~ 5000,
          "Little"              ~ 5,
          "Some"                ~ 3,
      .default              = NA_real_
    )
  ) %>%
  left_join(
    trees %>% select(-common_name, -species),
    by = "individual_id"
  )



# -----------------------------
# Weekly observer summaries
# -----------------------------
d_obs_weekly <- d %>%
  mutate(datedif = as.numeric(submission_date - observation_date)) %>%
  group_by(semester, observedby_person_id, observation_week) %>%
  summarize(
    n_observations  = n(),
    n_individuals   = n_distinct(individual_id, na.rm = TRUE),
    p_observations  = 100 * n_observations / require_obs_per_week,
    median_date_dif = median(datedif, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    semester,
    observedby_person_id,
    observation_week,
    fill = list(n_observations = 0, p_observations = 0)
  ) %>%
  group_by(semester, observedby_person_id) %>%
  arrange(observation_week) %>%
  mutate(
    obs7 = rollapply(
      n_observations, 3, mean,
      align = "center", na.rm = TRUE, partial = TRUE
    ),
    n_observations_filled =
      if_else(observation_week < 4 & obs7 > 3, obs7, n_observations)
  ) %>%
  ungroup()

# -----------------------------
# Semester-level summaries
# -----------------------------
d_obs <- d_obs_weekly %>%
  group_by(semester, observedby_person_id) %>%
  summarize(
    obs_week_count        = sum(n_observations > 0),
    obs_week_filled_count = sum(n_observations >= 2.6),
    obs_week_percent      = mean(p_observations),
    median_date_dif       = median(median_date_dif, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent = 100 * pmax(obs_week_count, obs_week_filled_count) / required_weeks,
    percent = pmin(percent, 100),
    percent = replace_na(percent, 0)
  )

# -----------------------------
# Save derived outputs
# -----------------------------
write_parquet(d,            file.path(base_dir, "full_data.parquet"))
write_parquet(trees,        file.path(base_dir, "trees.parquet"))
write_parquet(d_obs_weekly, file.path(base_dir, "weekly_observer_stats.parquet"))
write_parquet(d_obs,        file.path(base_dir, "semester_observer_stats.parquet"))
