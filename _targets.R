library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "tidyverse",
    "rnpn",
    "lubridate",
    "zoo",
    "arrow",
    "piggyback",
    "glue",
    "rmarkdown",
    "tarchetypes",
    "quarto"
  )
)

source("R/targets_pipeline.R")

quarto_main_files <- c(
  "index.qmd",
  "analysis.qmd",
  "semesters.qmd"
)

list(
  tar_target(
    config,
    cp_config(),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    dirs,
    cp_ensure_dirs(c(config$cache_dir, config$processed_dir, config$csv_dir, config$generated_dir)),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    trees_csv,
    "data/trees.csv",
    format = "file"
  ),
  tar_target(
    trees,
    cp_read_trees(trees_csv)
  ),
  tar_target(
    current_semester,
    cp_current_semester(Sys.Date()),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    ensure_release,
    cp_ensure_release(tag = config$release_tag, repo = config$repo),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    release_assets,
    cp_pb_list_safe(tag = config$release_tag, repo = config$repo),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    available_semesters,
    cp_available_semesters_from_assets(release_assets),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    expected_semesters,
    cp_expected_semesters(config$year_start, config$year_stop)
  ),
  tar_target(
    missing_semesters,
    cp_missing_semesters(
      expected_semesters = expected_semesters,
      available_semesters = available_semesters,
      current_semester = current_semester,
      cache_dir = config$cache_dir,
      network_id = config$network_id
    ),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    downloaded_release_assets,
    cp_pb_download_missing_assets(
      release_assets = release_assets,
      tag = config$release_tag,
      repo = config$repo,
      dest = config$cache_dir
    ),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    years_to_download,
    unique(floor(as.numeric(missing_semesters)))
  ),
  tar_target(
    raw_new,
    cp_npn_download_status_data_safe(
      years = years_to_download,
      request_source = config$request_source,
      network_id = config$network_id
    )
  ),
  tar_target(
    written_semester_parquets,
    cp_write_semester_parquets_and_upload(
      raw_new = raw_new,
      missing_semesters = missing_semesters,
      cache_dir = config$cache_dir,
      network_id = config$network_id,
      release_tag = config$release_tag,
      repo = config$repo
    ),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
      semester_parquets,
      unique(c(cp_list_semester_parquets(config$cache_dir), written_semester_parquets)),
      format = "file",
      cue = tar_cue(mode = "always")
    ),
  tar_target(
    d,
    cp_build_full_dataset(semester_parquets, trees)
  ),
  tar_target(
    d_obs_weekly,
    cp_weekly_observer_stats(d, require_obs_per_week = config$require_obs_per_week)
  ),
  tar_target(
    d_obs,
    cp_semester_observer_stats(d_obs_weekly, required_weeks = config$required_weeks)
  ),
  tar_target(
    full_data_parquet,
    cp_write_parquet_file(d, file.path(config$processed_dir, "full_data.parquet")),
    format = "file"
  ),
  tar_target(
    trees_parquet,
    cp_write_parquet_file(trees, file.path(config$processed_dir, "trees.parquet")),
    format = "file"
  ),
  tar_target(
    weekly_observer_stats_parquet,
    cp_write_parquet_file(d_obs_weekly, file.path(config$processed_dir, "weekly_observer_stats.parquet")),
    format = "file"
  ),
  tar_target(
    semester_observer_stats_parquet,
    cp_write_parquet_file(d_obs, file.path(config$processed_dir, "semester_observer_stats.parquet")),
    format = "file"
  ),
  tar_target(
    observer_semester_summary,
    cp_observer_semester_summary(d, d_obs)
  ),
  tar_target(
    exported_csv_files,
    cp_export_semester_csvs_and_upload(
      d = d,
      observer_semester_summary = observer_semester_summary,
      csv_dir = config$csv_dir,
      network_id = config$network_id,
      release_tag = config$release_tag,
      repo = config$repo
    ),
    format = "file"
  ),
  tar_target(
    semester_index,
    cp_semester_index(
      d = d,
      current_semester = current_semester,
      repo = config$repo,
      release_tag = config$release_tag,
      network_id = config$network_id
    )
  ),
  tar_target(
    semesters_qmd,
    cp_write_semesters_qmd(semester_index, path = "semesters.qmd"),
    format = "file"
  ),
  tar_target(
    semester_qmds,
    cp_generate_semester_qmds(
      semesters = sort(unique(d$semester)),
      required_weeks = config$required_weeks,
      require_obs_per_week = config$require_obs_per_week,
      generated_dir = config$generated_dir
    ),
    format = "file"
  ),
  tar_target(
    semester_for_students,
    cp_semester_for_students(d, current_semester)
  ),
  tar_target(
    student_qmds,
    cp_generate_student_qmds(
      d = d,
      semester_for_students = semester_for_students,
      required_weeks = config$required_weeks,
      require_obs_per_week = config$require_obs_per_week,
      generated_dir = config$generated_dir
    ),
    format = "file"
  ),
  tar_target(
    tree_qmds,
    cp_generate_tree_qmds(d = d, trees = trees, generated_dir = config$generated_dir),
    format = "file"
  )#,
  # Quarto rendering target
  #tar_quarto(website)
)
