###############################################################################
# PRE-RENDER SCRIPT FOR CAMPUS PHENOLOGY QUARTO SITE
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
library(rmarkdown)

# -------------------------------------------------------------------
# User-configurable parameters
# -------------------------------------------------------------------
year_start <- 2020
year_stop  <- year(Sys.Date())

network_id <- 891
repo       <- "AdamWilsonLabEDU/campus_phenology"
release_tag <- "npn-data"

required_weeks       <- 9
require_obs_per_week <- 4
require_obs          <- required_weeks * require_obs_per_week

current_semester <- semester(Sys.Date(), with_year = TRUE)

cache_dir <- "data/cache"
base_dir  <- "data/processed"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)


csv_dir <- file.path(cache_dir, "csv")
dir.create(csv_dir, showWarnings = FALSE)

generated_dir <- "generated"
dir.create(generated_dir, showWarnings = FALSE)

# -------------------------------------------------------------------
# Ensure GitHub Release Exists
# -------------------------------------------------------------------
release_exists <- TRUE
tryCatch(
  { piggyback::pb_list(tag = release_tag, repo = repo) },
  error = function(e) { release_exists <<- FALSE }
)

if (!release_exists) {
  piggyback::pb_release_create(
    tag = release_tag,
    repo = repo,
    name = release_tag,
    body = paste(
      "Automated data release for USA-NPN observations.",
      "Assets are semester-partitioned Parquet and CSV files.",
      "Current semester is refreshed on each run.",
      sep = "\n"
    )
  )
}

# -------------------------------------------------------------------
# Load tree metadata
# -------------------------------------------------------------------
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
# Determine expected semesters
# -------------------------------------------------------------------
expected_semesters <- expand_grid(
  year = year_start:year_stop,
  term = c(1, 2)
) %>%
  mutate(semester = paste(year, term, sep = ".")) %>%
  pull(semester)

closed_semesters <- setdiff(expected_semesters, current_semester)

# -------------------------------------------------------------------
# Query GitHub Release assets
# -------------------------------------------------------------------
release_assets <- piggyback::pb_list(tag = release_tag, repo = repo)

available_semesters <- release_assets$file_name %>%
  str_extract("semester-[0-9]{4}\\.[12]") %>%
  str_remove("semester-") %>%
  na.omit()

missing_semesters <- union(
  setdiff(closed_semesters, available_semesters),
  current_semester
)

# Check if current semester was recently downloaded; skip if < 1 day old
current_sem_file <- list.files(
  cache_dir,
  pattern = glue("npn_obs_network-{network_id}_semester-{current_semester}\\.parquet"),
  full.names = TRUE
)

if (length(current_sem_file) > 0 && file.exists(current_sem_file)) {
  file_mtime <- file.mtime(current_sem_file)
  time_since_download <- as.numeric(difftime(Sys.time(), file_mtime, units = "days"))
  
  if (time_since_download < 1) {
    message("Current semester data is fresh (<1 day old); skipping download.")
    missing_semesters <- setdiff(missing_semesters, current_semester)
  }
}

message("Semesters missing from GitHub Release: ",
        ifelse(length(missing_semesters) == 0, "none", paste(missing_semesters, collapse = ", ")))

# -------------------------------------------------------------------
# Download existing Parquet assets
# -------------------------------------------------------------------
if (length(release_assets) > 0) {
  piggyback::pb_download(tag = release_tag, repo = repo, dest = cache_dir)
}

# -------------------------------------------------------------------
# Download missing semesters from NPN
# -------------------------------------------------------------------
if (length(missing_semesters) > 0) {
  years_to_download <- unique(floor(as.numeric(missing_semesters)))
  raw_new <- npn_download_status_data(
    request_source    = "campus_phenology",
    years             = as.character(years_to_download),
    additional_fields = c("Plant_Nickname","ObservedBy_Person_ID","Submission_Datetime"),
    network_ids = network_id
  ) %>%
    as_tibble() %>%
    mutate(
      observation_date = ymd(observation_date),
      semester = sprintf("%d.%d", year(observation_date), semester(observation_date))
    ) %>%
    filter(semester %in% missing_semesters)

  # Split by semester and persist
  split(raw_new, raw_new$semester) %>%
    walk(function(df) {
      sem <- unique(df$semester)
      out_file <- file.path(cache_dir, glue("npn_obs_network-{network_id}_semester-{sem}.parquet"))
      write_parquet(df, out_file)
      piggyback::pb_upload(file = out_file, tag = release_tag, repo = repo, overwrite = TRUE)
    })
}

# -------------------------------------------------------------------
# Load all Parquet data
# -------------------------------------------------------------------
obs_files <- list.files(cache_dir, pattern = "npn_obs_network-.*\\.parquet", full.names = TRUE)
obs_ds <- open_dataset(obs_files)

d <- obs_ds %>% collect() %>%
  mutate(
    submission_date   = as.Date(submission_datetime),
    observation_week  = isoweek(observation_date),
    observation_doy   = yday(observation_date),
    observation_year  = year(observation_date),
    semester          = as.character(semester),
    observation_datey = make_date(2020, month(observation_date), day(observation_date)),
    observedby_person_id = factor(observedby_person_id)
  ) %>%
  rename(intensity_raw = intensity_value) %>%
  mutate(
    intensity = case_match(
      intensity_raw,
      "Less than 5%" ~ 2.5,
      "Less than 25%" ~ 20,
      "5-24%" ~ 14.5,
      "25-49%" ~ 37,
      "50-74%" ~ 62,
      "75-94%" ~ 84.5,
      "95% or more" ~ 97.5,
      "Less than 3" ~ 2,
      "3 to 10" ~ 6.5,
      "11 to 100" ~ 50,
      "101 to 1,000" ~ 500,
      "1,001 to 10,000" ~ 5000,
      "Little" ~ 5,
      "Some" ~ 3,
      .default = NA_real_
    )
  ) %>%
  left_join(trees %>% select(-common_name, -species), by = "individual_id")

# -------------------------------------------------------------------
# Weekly observer summaries
# -------------------------------------------------------------------
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
    obs7 = rollapply(n_observations, 3, mean, align = "center", na.rm = TRUE, partial = TRUE),
    n_observations_filled = if_else(observation_week < 4 & obs7 > 3, obs7, n_observations)
  ) %>%
  ungroup()

# -------------------------------------------------------------------
# Semester summaries
# -------------------------------------------------------------------
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

# -------------------------------------------------------------------
# Save core outputs
# -------------------------------------------------------------------
write_parquet(d,            file.path(base_dir, "full_data.parquet"))
write_parquet(trees,        file.path(base_dir, "trees.parquet"))
write_parquet(d_obs_weekly, file.path(base_dir, "weekly_observer_stats.parquet"))
write_parquet(d_obs,        file.path(base_dir, "semester_observer_stats.parquet"))

# -------------------------------------------------------------------
# Export CSVs per semester + upload to GitHub Release
# -------------------------------------------------------------------
walk(unique(d$semester), function(sem) {
  csv_file <- file.path(csv_dir, glue("npn_obs_network-{network_id}_semester-{sem}.csv"))
  d %>% filter(semester == sem) %>% arrange(observation_date) %>% write_csv(csv_file)
  piggyback::pb_upload(file = csv_file, tag = release_tag, repo = repo, overwrite = TRUE)
})

# -------------------------------------------------------------------
# Semester-level stats
# -------------------------------------------------------------------
semester_stats <- d %>%
  group_by(semester) %>%
  summarize(
    n_observers    = n_distinct(observedby_person_id),
    n_observations = n(),
    semester_start = min(observation_date, na.rm = TRUE),
    semester_stop  = max(observation_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(semester)) %>%
  mutate(is_current = semester == current_semester)

semester_index <- semester_stats %>%
  mutate(
    year = substr(semester, 1, 4),
    term = substr(semester, 6, 6),
    label = case_when(
      term == "1" ~ paste("Spring", year),
      term == "2" ~ paste("Fall", year),
      TRUE        ~ semester
    ),
    status = if_else(is_current, "In progress", "Complete"),
    page_href = glue("generated/semester_{semester}.html"),
    csv_href  = glue(
      "https://github.com/{repo}/releases/download/{release_tag}/",
      "npn_obs_network-{network_id}_semester-{semester}.csv"
    )
  )

# -------------------------------------------------------------------
# Generate semesters.qmd
# -------------------------------------------------------------------
semester_table <- semester_index %>%
  transmute(
    Semester     = label,
    Status       = status,
    Observers    = n_observers,
    Observations = n_observations,
    Page         = glue("[View]({page_href})"),
    Data         = glue("[CSV]({csv_href})")
  )

md_table <- paste0(
  "| ", paste(names(semester_table), collapse = " | "), " |\n",
  "|", paste(rep("---", ncol(semester_table)), collapse = "|"), "|\n",
  paste(
    apply(semester_table, 1, function(row) paste(row, collapse = " | ")),
    collapse = "\n"
  )
)

writeLines(
  c(
    "---",
    "title: \"Semester Data\"",
    "format:",
    "  html:",
    "    toc: false",
    "---",
    "",
    "This page lists all semesters with available observation data.",
    "",
    md_table
  ),
  "semesters.qmd"
)

# -------------------------------------------------------------------
# Generate Semester QMD Files (Quarto will render them)
# -------------------------------------------------------------------
walk(unique(d$semester), function(sem) {
  ds_sem <- d %>% filter(semester == sem)
  ds_obs <- d_obs %>% filter(semester == sem)
  ds_obs_weekly <- d_obs_weekly %>% filter(semester == sem)
  if (nrow(ds_sem) == 0) return()

  qmd_file <- file.path(generated_dir, glue("semester_{sem}.qmd"))
  
  # Save data objects as parquet for retrieval at render time
  ds_sem_file <- file.path(generated_dir, glue(".semester_{sem}_ds.parquet"))
  ds_obs_file <- file.path(generated_dir, glue(".semester_{sem}_ds_obs.parquet"))
  ds_obs_weekly_file <- file.path(generated_dir, glue(".semester_{sem}_ds_obs_weekly.parquet"))
  
  write_parquet(ds_sem, ds_sem_file)
  write_parquet(ds_obs, ds_obs_file)
  write_parquet(ds_obs_weekly, ds_obs_weekly_file)
  
  # Read template and write with embedded params
  template_content <- readLines("template/semester_template.qmd")
  
  # Inject params into YAML frontmatter
  new_content <- c(
    "---",
    "title: \"Semester Data\"",
    "format:",
    "  html:",
    "    toc: true",
    "    number-sections: true",
    "params:",
    glue("  semester: {sem}"),
    glue("  ds_file: '{ds_sem_file}'"),
    glue("  ds_obs_file: '{ds_obs_file}'"),
    glue("  ds_obs_weekly_file: '{ds_obs_weekly_file}'"),
    glue("  required_weeks: {required_weeks}"),
    glue("  require_obs_per_week: {require_obs_per_week}"),
    "editor_options:",
    "  chunk_output_type: console",
    "---",
    "",
    "```{r echo=F}",
    "library(tidyverse)",
    "library(DT)",
    "library(ggplot2)",
    "library(arrow)",
    "ds <- read_parquet(params$ds_file)",
    "ds_obs <- read_parquet(params$ds_obs_file)",
    "ds_obs_weekly <- read_parquet(params$ds_obs_weekly_file)",
    "```",
    "",
    # Rest of template (skip header)
    template_content[(which(template_content == "```")[2] + 1):length(template_content)]
  )
  
  writeLines(new_content, qmd_file)
  message("Generated semester QMD: ", qmd_file)
})

# -------------------------------------------------------------------
# Generate Student QMD Files (current semester only; Quarto will render them)
# -------------------------------------------------------------------
ds_sem <- d %>% filter(semester == current_semester)
nnids <- unique(ds_sem$observedby_person_id)

walk(nnids, function(nnid) {
  qmd_file <- file.path(generated_dir, glue("student_{current_semester}_{nnid}.qmd"))
  
  # Save data object as parquet for retrieval at render time
  ds_student_file <- file.path(generated_dir, glue(".student_{current_semester}_{nnid}_ds.parquet"))
  ds_student <- ds_sem %>% filter(observedby_person_id == nnid)
  write_parquet(ds_student, ds_student_file)
  
  # Read template and write with embedded params
  template_content <- readLines("template/student_template.qmd")
  
  # Inject params into YAML frontmatter
  new_content <- c(
    "---",
    "title: \"Student Observations\"",
    "format:",
    "  html:",
    "    toc: true",
    "    number-sections: true",
    "params:",
    glue("  semester: {current_semester}"),
    glue("  nnid: {nnid}"),
    glue("  ds_file: '{ds_student_file}'"),
    glue("  required_weeks: {required_weeks}"),
    glue("  require_obs_per_week: {require_obs_per_week}"),
    "---",
    "",
    "```{r echo=F}",
    "library(tidyverse)",
    "library(ggplot2)",
    "library(arrow)",
    "ds <- read_parquet(params$ds_file)",
    "```",
    "",
    # Rest of template (skip header)
    template_content[(which(template_content == "```")[2] + 1):length(template_content)]
  )
  
  writeLines(new_content, qmd_file)
  message("Generated student QMD: ", qmd_file)
})

# -------------------------------------------------------------------
# Generate Tree QMD Files (all trees with observations)
# -------------------------------------------------------------------
tree_list <- d %>%
  filter(!is.na(individual_id)) %>%
  group_by(individual_id) %>%
  summarize(
    common_name = first(common_name),
    plant_nickname = first(plant_nickname),
    .groups = "drop"
  ) %>%
  left_join(trees %>% select(individual_id, tag, lat, lon), by = "individual_id") %>%
  filter(!is.na(tag))

walk(seq_len(nrow(tree_list)), function(i) {
  tree_info <- tree_list[i, ]
  tree_id <- tree_info$individual_id
  
  # Save data objects as parquet for retrieval at render time
  ds_tree_file <- file.path(generated_dir, glue(".tree_{tree_id}_ds.parquet"))
  ds_tree <- d %>% filter(individual_id == tree_id)
  write_parquet(ds_tree, ds_tree_file)
  
  # All observations for same species
  ds_all_file <- file.path(generated_dir, glue(".tree_{tree_id}_ds_all.parquet"))
  ds_all <- d %>% filter(common_name == tree_info$common_name)
  write_parquet(ds_all, ds_all_file)
  
  # Tree location data with plant nicknames
  ds_trees_file <- file.path(generated_dir, glue(".tree_{tree_id}_ds_trees.parquet"))
  trees_with_nicknames <- d %>%
    group_by(individual_id) %>%
    summarize(plant_nickname = first(plant_nickname), .groups = "drop") %>%
    right_join(trees, by = "individual_id")
  write_parquet(trees_with_nicknames, ds_trees_file)
  
  # Read template and write with embedded params
  template_content <- readLines("template/tree_template.qmd")
  
  # Inject params into YAML frontmatter
  new_content <- c(
    "---",
    "title: \"Tree Observations\"",
    "format:",
    "  html:",
    "    toc: true",
    "    number-sections: true",
    "params:",
    glue("  tree_id: {tree_id}"),
    glue("  common_name: '{tree_info$common_name}'"),
    glue("  plant_nickname: '{tree_info$plant_nickname}'"),
    glue("  tag: '{tree_info$tag}'"),
    glue("  ds_file: '{ds_tree_file}'"),
    glue("  ds_all_file: '{ds_all_file}'"),
    glue("  ds_trees_file: '{ds_trees_file}'"),
    "editor_options:",
    "  chunk_output_type: console",
    "---",
    "",
    "```{r echo=F}",
    "library(tidyverse)",
    "library(leaflet)",
    "library(ggplot2)",
    "library(arrow)",
    "ds <- read_parquet(params$ds_file)",
    "ds_all <- read_parquet(params$ds_all_file)",
    "ds_trees <- read_parquet(params$ds_trees_file)",
    "```",
    "",
    # Rest of template (skip header)
    template_content[(which(template_content == "```")[2] + 1):length(template_content)]
  )
  
  qmd_file <- file.path(generated_dir, glue("tree_{tree_id}.qmd"))
  writeLines(new_content, qmd_file)
  message("Generated tree QMD: ", qmd_file)
})
