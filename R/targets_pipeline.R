cp_config <- function() {
  list(
    year_start = 2020,
    year_stop = lubridate::year(Sys.Date()),
    network_id = 891,
    repo = "AdamWilsonLabEDU/campus_phenology",
    release_tag = "npn-data",
    required_weeks = 9,
    require_obs_per_week = 4,
    request_source = "campus_phenology",
    cache_dir = "data/cache",
    processed_dir = "data/processed",
    csv_dir = file.path("data/cache", "csv"),
    generated_dir = "generated"
  )
}

cp_ensure_dirs <- function(paths) {
  purrr::walk(paths, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))
  invisible(TRUE)
}

safe_pb_upload <- function(file, tag, repo, overwrite = TRUE) {
  tryCatch(
    {
      piggyback::pb_upload(file = file, tag = tag, repo = repo, overwrite = overwrite)
      TRUE
    },
    error = function(e) {
      message(
        "Skipping GitHub upload for ", basename(file),
        ". Error: ", conditionMessage(e)
      )
      FALSE
    }
  )
}

safe_pb_release_create <- function(tag, repo, name, body) {
  tryCatch(
    {
      piggyback::pb_release_create(tag = tag, repo = repo, name = name, body = body)
      TRUE
    },
    error = function(e) {
      message("Could not create GitHub release '", tag, "'. Error: ", conditionMessage(e))
      FALSE
    }
  )
}

cp_pb_list_safe <- function(tag, repo) {
  tryCatch(
    piggyback::pb_list(tag = tag, repo = repo),
    error = function(e) tibble::tibble(file_name = character())
  )
}

cp_ensure_release <- function(tag, repo) {
  release_exists <- TRUE
  tryCatch(
    { piggyback::pb_list(tag = tag, repo = repo) },
    error = function(e) { release_exists <<- FALSE }
  )

  if (!release_exists) {
    safe_pb_release_create(
      tag = tag,
      repo = repo,
      name = tag,
      body = paste(
        "Automated data release for USA-NPN observations.",
        "Assets are semester-partitioned Parquet and CSV files.",
        "Current semester is refreshed on each run.",
        sep = "\n"
      )
    )
  }

  invisible(TRUE)
}

cp_current_semester <- function(date = Sys.Date()) {
  lubridate::semester(date, with_year = TRUE)
}

cp_read_trees <- function(trees_csv) {
  readr::read_csv(trees_csv, show_col_types = FALSE) %>%
    dplyr::mutate(
      common_name = dplyr::case_match(
        common_name,
        "Red Oak" ~ "northern red oak",
        "Black Oak" ~ "black oak",
        "Silver Maple" ~ "silver maple",
        "Red Maple" ~ "red maple",
        "Honey Locust" ~ "honeylocust",
        "Sugar Maple" ~ "sugar maple",
        "Staghorn Sumac" ~ "staghorn sumac",
        "Eastern Cottonwood" ~ "eastern cottonwood",
        "Apple" ~ "apple",
        "American Basswood" ~ "American basswood",
        "River Birch" ~ "river birch",
        "White Birch" ~ "paper birch",
        "Ginkgo" ~ "maidenhair tree",
        "Black Locust" ~ "black locust",
        .default = tolower(common_name)
      ),
      color = dplyr::case_match(
        common_name,
        "northern red oak" ~ "red",
        "black oak" ~ "black",
        "silver maple" ~ "darkblue",
        "red maple" ~ "orange",
        "honeylocust" ~ "purple",
        "sugar maple" ~ "darkpurple",
        "staghorn sumac" ~ "blue",
        "eastern cottonwood" ~ "beige",
        "apple" ~ "yellow",
        "American basswood" ~ "pink",
        "river birch" ~ "lightred",
        "paper birch" ~ "cadetblue",
        "maidenhair tree" ~ "lightgray",
        "black locust" ~ "darkgreen",
        .default = NA_character_
      ),
      common_name_tag = paste(tag, common_name)
    ) %>%
    dplyr::select(-OBJECTID)
}

cp_expected_semesters <- function(year_start, year_stop) {
  tidyr::expand_grid(
    year = year_start:year_stop,
    term = c(1, 2)
  ) %>%
    dplyr::mutate(semester = paste(year, term, sep = ".")) %>%
    dplyr::pull(semester)
}

cp_available_semesters_from_assets <- function(release_assets) {
  release_assets$file_name %>%
    stringr::str_extract("semester-[0-9]{4}\\.[12]") %>%
    stringr::str_remove("semester-") %>%
    stats::na.omit() %>%
    as.character()
}

cp_missing_semesters <- function(expected_semesters, available_semesters, current_semester, cache_dir, network_id) {
  closed_semesters <- setdiff(expected_semesters, current_semester)

  missing_semesters <- union(
    setdiff(closed_semesters, available_semesters),
    current_semester
  )

  current_sem_file <- list.files(
    cache_dir,
    pattern = glue::glue("npn_obs_network-{network_id}_semester-{current_semester}\\.parquet"),
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

  missing_semesters
}

cp_pb_download_missing_assets <- function(release_assets, tag, repo, dest) {
  if (!is.data.frame(release_assets) || nrow(release_assets) == 0) {
    return(invisible(character()))
  }

  remote_files <- unique(release_assets$file_name)
  remote_files <- remote_files[!is.na(remote_files)]
  if (length(remote_files) == 0) return(invisible(character()))

  local_paths <- file.path(dest, remote_files)
  to_get <- remote_files[!file.exists(local_paths)]

  if (length(to_get) == 0) {
    message("All release assets already present in cache.")
    return(invisible(local_paths[file.exists(local_paths)]))
  }

  piggyback::pb_download(tag = tag, repo = repo, dest = dest, file = to_get)
  invisible(file.path(dest, to_get))
}

cp_npn_download_status_data_safe <- function(years, request_source, network_id) {
  if (length(years) == 0) {
    return(tibble::tibble())
  }

  raw_download <- tryCatch(
    {
      rnpn::npn_download_status_data(
        request_source = request_source,
        years = as.character(years),
        additional_fields = c("Plant_Nickname", "ObservedBy_Person_ID", "Submission_Datetime"),
        network_ids = network_id
      )
    },
    error = function(e) {
      message("NPN download failed; skipping missing semesters. Error: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(raw_download)) {
    return(tibble::tibble())
  }

  tibble::as_tibble(raw_download)
}

cp_write_semester_parquets_and_upload <- function(raw_new, missing_semesters, cache_dir, network_id, release_tag, repo) {
  if (!is.data.frame(raw_new) || nrow(raw_new) == 0 || !("observation_date" %in% names(raw_new))) {
    return(character())
  }

  raw_new <- raw_new %>%
    dplyr::mutate(
      observation_date = lubridate::ymd(observation_date),
      semester = sprintf("%d.%d", lubridate::year(observation_date), lubridate::semester(observation_date))
    ) %>%
    dplyr::filter(semester %in% missing_semesters)

  if (nrow(raw_new) == 0) {
    return(character())
  }

  out_files <- character()

  split(raw_new, raw_new$semester) %>%
    purrr::walk(function(df) {
      sem <- unique(df$semester)
      out_file <- file.path(cache_dir, glue::glue("npn_obs_network-{network_id}_semester-{sem}.parquet"))
      arrow::write_parquet(df, out_file)
      safe_pb_upload(file = out_file, tag = release_tag, repo = repo, overwrite = TRUE)
      out_files <<- c(out_files, out_file)
    })

  out_files
}

cp_list_semester_parquets <- function(cache_dir) {
  files <- list.files(cache_dir, pattern = "npn_obs_network-.*\\.parquet", full.names = TRUE)
  cat("[cp_list_semester_parquets] Found", length(files), "Parquet files in", cache_dir, "\n")
  if (length(files) > 0) cat("[cp_list_semester_parquets] Files:\n", paste(basename(files), collapse = ", "), "\n")
  files
}

cp_build_full_dataset <- function(parquet_files, trees) {
  if (length(parquet_files) == 0) stop("No Parquet files found for dataset build.")
  obs_ds <- arrow::open_dataset(parquet_files)
  df <- obs_ds %>% dplyr::collect()
  if (nrow(df) == 0) stop("No rows in combined dataset.")
  print("Columns in combined dataset:")
  print(names(df))
  # Continue with the original pipeline
  df %>%
    dplyr::mutate(
      submission_date = as.Date(submission_datetime),
      observation_week = lubridate::isoweek(observation_date),
      observation_doy = lubridate::yday(observation_date),
      observation_year = lubridate::year(observation_date),
      semester = as.character(semester),
      observation_datey = lubridate::make_date(2020, lubridate::month(observation_date), lubridate::day(observation_date)),
      observedby_person_id = factor(observedby_person_id)
    ) %>%
    dplyr::rename(intensity_raw = intensity_value) %>%
    dplyr::mutate(
      intensity = dplyr::case_match(
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
    dplyr::left_join(trees %>% dplyr::select(-common_name, -species), by = "individual_id")
}

cp_weekly_observer_stats <- function(d, require_obs_per_week) {
  d %>%
    dplyr::mutate(datedif = as.numeric(submission_date - observation_date)) %>%
    dplyr::group_by(semester, observedby_person_id, observation_week) %>%
    dplyr::summarize(
      n_observations = dplyr::n(),
      n_individuals = dplyr::n_distinct(individual_id, na.rm = TRUE),
      p_observations = 100 * n_observations / require_obs_per_week,
      median_date_dif = stats::median(datedif, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(p_observations = pmin(p_observations, 100)) %>%
    tidyr::complete(
      semester,
      observedby_person_id,
      observation_week,
      fill = list(n_observations = 0, p_observations = 0)
    ) %>%
    dplyr::group_by(semester, observedby_person_id) %>%
    dplyr::arrange(observation_week) %>%
    dplyr::mutate(
      obs7 = zoo::rollapply(n_observations, 3, mean, align = "center", na.rm = TRUE, partial = TRUE),
      n_observations_filled = dplyr::if_else(observation_week < 4 & obs7 > 3, obs7, n_observations)
    ) %>%
    dplyr::ungroup()
}

cp_semester_observer_stats <- function(d_obs_weekly, required_weeks) {
  d_obs_weekly %>%
    dplyr::group_by(semester, observedby_person_id) %>%
    dplyr::summarize(
      obs_week_count = sum(n_observations > 0),
      obs_week_filled_count = sum(n_observations >= 2.6),
      obs_week_percent = mean(p_observations),
      median_date_dif = stats::median(median_date_dif, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      obs_week_percent = pmin(obs_week_percent, 100),
      percent = 100 * pmax(obs_week_count, obs_week_filled_count) / required_weeks,
      percent = pmin(percent, 100),
      percent = tidyr::replace_na(percent, 0)
    )
}

cp_write_parquet_file <- function(df, path) {
  cp_ensure_dirs(dirname(path))
  arrow::write_parquet(df, path)
  path
}

cp_observer_semester_summary <- function(d, d_obs) {
  d %>%
    dplyr::group_by(semester, observedby_person_id) %>%
    dplyr::summarize(
      total_observations = dplyr::n(),
      first_observation_date = min(observation_date, na.rm = TRUE),
      last_observation_date = max(observation_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      d_obs %>% dplyr::select(semester, observedby_person_id, obs_week_percent, median_date_dif),
      by = c("semester", "observedby_person_id")
    ) %>%
    dplyr::mutate(NNID = as.character(observedby_person_id)) %>%
    dplyr::select(
      semester,
      NNID,
      obs_week_percent,
      median_date_dif,
      total_observations,
      first_observation_date,
      last_observation_date
    )
}

cp_export_semester_csvs_and_upload <- function(d, observer_semester_summary, csv_dir, network_id, release_tag, repo) {
  cp_ensure_dirs(csv_dir)

  semesters <- unique(d$semester)
  if (length(semesters) == 0) return(character())

  out_files <- character()

  purrr::walk(semesters, function(sem) {
    csv_file <- file.path(csv_dir, glue::glue("npn_obs_network-{network_id}_semester-{sem}.csv"))
    d %>%
      dplyr::filter(semester == sem) %>%
      dplyr::arrange(observation_date) %>%
      readr::write_csv(csv_file)
    safe_pb_upload(file = csv_file, tag = release_tag, repo = repo, overwrite = TRUE)
    out_files <<- c(out_files, csv_file)

    observer_csv_file <- file.path(csv_dir, glue::glue("npn_obs_network-{network_id}_semester-{sem}_observer_summary.csv"))
    observer_semester_summary %>%
      dplyr::filter(semester == sem) %>%
      dplyr::select(-semester) %>%
      dplyr::arrange(NNID) %>%
      readr::write_csv(observer_csv_file)
    safe_pb_upload(file = observer_csv_file, tag = release_tag, repo = repo, overwrite = TRUE)
    out_files <<- c(out_files, observer_csv_file)
  })

  out_files
}

cp_semester_index <- function(d, current_semester, repo, release_tag, network_id) {
  semester_stats <- d %>%
    dplyr::group_by(semester) %>%
    dplyr::summarize(
      n_observers = dplyr::n_distinct(observedby_person_id),
      n_observations = dplyr::n(),
      semester_start = min(observation_date, na.rm = TRUE),
      semester_stop = max(observation_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(semester)) %>%
    dplyr::mutate(is_current = semester == current_semester)

  semester_stats %>%
    dplyr::mutate(
      year = substr(semester, 1, 4),
      term = substr(semester, 6, 6),
      label = dplyr::case_when(
        term == "1" ~ paste("Spring", year),
        term == "2" ~ paste("Fall", year),
        TRUE ~ semester
      ),
      status = dplyr::if_else(is_current, "In progress", "Complete"),
      page_href = glue::glue("generated/semester_{semester}.html"),
      csv_href = glue::glue(
        "https://github.com/{repo}/releases/download/{release_tag}/",
        "npn_obs_network-{network_id}_semester-{semester}.csv"
      ),
      observer_csv_href = glue::glue(
        "https://github.com/{repo}/releases/download/{release_tag}/",
        "npn_obs_network-{network_id}_semester-{semester}_observer_summary.csv"
      )
    )
}

cp_write_semesters_qmd <- function(semester_index, path = "semesters.qmd") {
  semester_table <- semester_index %>%
    dplyr::transmute(
      Semester = label,
      Status = status,
      Observers = n_observers,
      Observations = n_observations,
      Page = glue::glue("[View]({page_href})"),
      Data = glue::glue("[Observations CSV]({csv_href})"),
      ObserverData = glue::glue("[Observer summary CSV]({observer_csv_href})")
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
    path
  )

  path
}

cp_generate_from_template_yaml_replace <- function(template_path, replacements, out_path) {
  template_content <- readLines(template_path)

  yaml_start <- which(template_content == "---")[1]
  yaml_end <- which(template_content == "---")[2]

  yaml_lines <- template_content[yaml_start:yaml_end]
  for (pattern in names(replacements)) {
    yaml_lines <- gsub(pattern, replacements[[pattern]], yaml_lines)
  }

  new_content <- c(yaml_lines, template_content[(yaml_end + 1):length(template_content)])
  cp_ensure_dirs(dirname(out_path))
  writeLines(new_content, out_path)
  out_path
}

cp_generate_semester_qmds <- function(semesters, required_weeks, require_obs_per_week, generated_dir) {
  if (length(semesters) == 0) return(character())

  purrr::map_chr(semesters, function(sem) {
    out_path <- file.path(generated_dir, glue::glue("semester_{sem}.qmd"))

    cp_generate_from_template_yaml_replace(
      template_path = "template/semester_template.qmd",
      replacements = list(
        "^  semester: null$" = paste0("  semester: ", sem),
        "^  required_weeks: 9$" = paste0("  required_weeks: ", required_weeks),
        "^  require_obs_per_week: 4$" = paste0("  require_obs_per_week: ", require_obs_per_week)
      ),
      out_path = out_path
    )
  })
}

cp_semester_for_students <- function(d, current_semester) {
  available_semesters_in_data <- d %>%
    dplyr::distinct(semester) %>%
    dplyr::mutate(
      year = as.integer(substr(semester, 1, 4)),
      term = as.integer(substr(semester, 6, 6))
    ) %>%
    dplyr::arrange(year, term)

  if (nrow(available_semesters_in_data) == 0) return(current_semester)

  if (current_semester %in% available_semesters_in_data$semester) {
    current_semester
  } else {
    available_semesters_in_data$semester[[nrow(available_semesters_in_data)]]
  }
}

cp_generate_student_qmds <- function(d, semester_for_students, required_weeks, require_obs_per_week, generated_dir) {
  ds_sem <- d %>% dplyr::filter(semester == semester_for_students)
  if (nrow(ds_sem) == 0) return(character())

  nnids <- unique(ds_sem$observedby_person_id)

  purrr::map_chr(nnids, function(nnid) {
    out_path <- file.path(generated_dir, glue::glue("student_{semester_for_students}_{nnid}.qmd"))

    cp_generate_from_template_yaml_replace(
      template_path = "template/student_template.qmd",
      replacements = list(
        "^  semester: null$" = paste0("  semester: ", semester_for_students),
        "^  nnid: null$" = paste0("  nnid: ", nnid),
        "^  required_weeks: 9$" = paste0("  required_weeks: ", required_weeks),
        "^  require_obs_per_week: 4$" = paste0("  require_obs_per_week: ", require_obs_per_week)
      ),
      out_path = out_path
    )
  })
}

cp_generate_tree_qmds <- function(d, trees, generated_dir) {
  tree_list <- d %>%
    dplyr::filter(!is.na(individual_id)) %>%
    dplyr::group_by(individual_id) %>%
    dplyr::summarize(
      common_name = dplyr::first(common_name),
      plant_nickname = dplyr::first(plant_nickname),
      .groups = "drop"
    ) %>%
    dplyr::left_join(trees %>% dplyr::select(individual_id, tag, lat, lon), by = "individual_id") %>%
    dplyr::filter(!is.na(tag))

  if (nrow(tree_list) == 0) return(character())

  purrr::map_chr(seq_len(nrow(tree_list)), function(i) {
    tree_info <- tree_list[i, ]
    tree_id <- tree_info$individual_id

    out_path <- file.path(generated_dir, glue::glue("tree_{tree_id}.qmd"))

    cp_generate_from_template_yaml_replace(
      template_path = "template/tree_template.qmd",
      replacements = list(
        "^  tree_id: null$" = paste0("  tree_id: ", tree_id),
        "^  common_name: null$" = paste0("  common_name: '", tree_info$common_name, "'"),
        "^  plant_nickname: null$" = paste0("  plant_nickname: '", tree_info$plant_nickname, "'"),
        "^  tag: null$" = paste0("  tag: '", tree_info$tag, "'"))
      ,
      out_path = out_path
    )
  })
}
