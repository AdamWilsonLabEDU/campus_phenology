# HLS EVI (HLSL30_VI + HLSS30_VI) via AppEEARS for campus tree points.
# Soft-fails without Earthdata credentials: uses Release tag `hls-evi`, then
# committed seed under fixtures/hls_evi/, then an empty schema Parquet so the
# site still builds.
#
# AppEEARS product IDs are HLSL30_VI.020 / HLSS30_VI.020 (Vegetation Indices
# v2.0). The Earthdata collection is often labeled ".002" / v2.0; AppEEARS uses
# ".020". The VI product already Fmask-masks cloud/shadow/adjacent to fill;
# "drop non-clear" here means drop fill and out-of-range EVI.

hls_evi_api_url <- "https://appeears.earthdatacloud.nasa.gov/api/"

hls_evi_products <- c(
  L30 = "HLSL30_VI.020",
  S30 = "HLSS30_VI.020"
)

hls_evi_fill_values <- c(-19999, -9999)
hls_evi_scale <- 0.0001
hls_evi_release_tag <- "hls-evi"
hls_evi_dir_default <- "data/hls_evi"
hls_evi_fixture_dir <- "fixtures/hls_evi"
hls_evi_mean_basename <- "hls_evi_campus_mean.parquet"
hls_evi_tree_basename <- "hls_evi_per_tree.parquet"

#' Resolve Earthdata / AppEEARS credentials from the environment.
#' Accepts EARTHDATA_USERNAME + EARTHDATA_PASSWORD, or APPEEARS_TOKEN.
hls_evi_credentials <- function() {
  token <- Sys.getenv("APPEEARS_TOKEN", unset = "")
  user <- Sys.getenv("EARTHDATA_USERNAME", unset = "")
  if (!nzchar(user)) {
    user <- Sys.getenv("EARTHDATA_USER", unset = "")
  }
  pass <- Sys.getenv("EARTHDATA_PASSWORD", unset = "")
  if (!nzchar(pass)) {
    pass <- Sys.getenv("EARTHDATA_PASS", unset = "")
  }
  list(
    token = if (nzchar(token)) token else NULL,
    username = if (nzchar(user)) user else NULL,
    password = if (nzchar(pass)) pass else NULL
  )
}

hls_evi_has_credentials <- function(creds = hls_evi_credentials()) {
  !is.null(creds$token) || (!is.null(creds$username) && !is.null(creds$password))
}

hls_evi_login <- function(creds = hls_evi_credentials(), api = hls_evi_api_url) {
  if (!is.null(creds$token)) {
    return(creds$token)
  }
  if (is.null(creds$username) || is.null(creds$password)) {
    stop("Missing Earthdata credentials for AppEEARS login.")
  }
  secret <- jsonlite::base64_enc(paste(creds$username, creds$password, sep = ":"))
  resp <- httr::POST(
    paste0(api, "login"),
    httr::add_headers(
      Authorization = paste("Basic", gsub("\n", "", secret)),
      `Content-Type` = "application/x-www-form-urlencoded;charset=UTF-8"
    ),
    body = "grant_type=client_credentials"
  )
  if (httr::status_code(resp) != 200) {
    stop(
      "AppEEARS login failed with HTTP ", httr::status_code(resp), ": ",
      httr::content(resp, as = "text", encoding = "UTF-8")
    )
  }
  httr::content(resp, as = "parsed", encoding = "UTF-8")$token
}

hls_evi_auth_header <- function(token) {
  httr::add_headers(Authorization = paste("Bearer", token))
}

#' Calendar years spanning NPN coverage (inclusive).
hls_evi_years <- function(year_start = 2019L, year_stop = lubridate::year(Sys.Date())) {
  as.integer(year_start):as.integer(year_stop)
}

hls_evi_empty_per_tree <- function() {
  tibble::tibble(
    date = as.Date(character()),
    doy = integer(),
    year = integer(),
    evi = numeric(),
    product = character(),
    sensor = character(),
    qa_ok = logical(),
    individual_id = integer(),
    lat = numeric(),
    lon = numeric(),
    source = character()
  )
}

hls_evi_empty_campus_mean <- function() {
  tibble::tibble(
    date = as.Date(character()),
    doy = integer(),
    year = integer(),
    evi_mean = numeric(),
    n_trees = integer(),
    n_clear = integer(),
    source = character()
  )
}

hls_evi_write_parquet <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Build a synthetic campus-mean EVI seed for offline builds / demos.
#' Shape is a simple seasonal curve (not real satellite values).
hls_evi_build_seed_mean <- function(years = 2023:2024, n_trees = 99L) {
  purrr::map_dfr(years, function(yr) {
    doys <- seq(1L, if (lubridate::leap_year(yr)) 366L else 365L, by = 3L)
    # Sparse "clear" samples with intentional gaps
    keep <- (doys %% 9L) != 0L
    doys <- doys[keep]
    evi <- 0.12 + 0.45 * pmax(
      0,
      sin(pi * pmax(0, pmin(1, (doys - 80) / 160))) *
        (1 - pmax(0, (doys - 260) / 80))
    )
    tibble::tibble(
      date = as.Date(doys - 1L, origin = paste0(yr, "-01-01")),
      doy = as.integer(doys),
      year = as.integer(yr),
      evi_mean = as.numeric(evi),
      n_trees = as.integer(n_trees),
      n_clear = as.integer(pmax(1L, round(n_trees * (0.55 + 0.35 * evi)))),
      source = "seed"
    )
  })
}

hls_evi_ensure_fixture <- function(fixture_dir = hls_evi_fixture_dir) {
  dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
  mean_path <- file.path(fixture_dir, hls_evi_mean_basename)
  csv_path <- file.path(fixture_dir, "hls_evi_campus_mean.csv")
  if (!file.exists(mean_path) && file.exists(csv_path)) {
    df <- readr::read_csv(csv_path, show_col_types = FALSE) |>
      dplyr::mutate(
        date = as.Date(.data$date),
        doy = as.integer(.data$doy),
        year = as.integer(.data$year),
        evi_mean = as.numeric(.data$evi_mean),
        n_trees = as.integer(.data$n_trees),
        n_clear = as.integer(.data$n_clear),
        source = as.character(.data$source)
      )
    hls_evi_write_parquet(df, mean_path)
  }
  if (!file.exists(mean_path)) {
    hls_evi_write_parquet(hls_evi_build_seed_mean(), mean_path)
  }
  mean_path
}

#' Drop non-clear / fill EVI samples (VI product already Fmask-fills clouds).
hls_evi_qc_clear <- function(df) {
  df |>
    dplyr::mutate(
      evi_raw = .data$evi_raw,
      evi = dplyr::if_else(
        is.na(.data$evi_raw) | .data$evi_raw %in% hls_evi_fill_values,
        NA_real_,
        as.numeric(.data$evi_raw) * hls_evi_scale
      ),
      qa_ok = !is.na(.data$evi) & .data$evi >= -1 & .data$evi <= 1
    ) |>
    dplyr::filter(.data$qa_ok)
}

#' Aggregate clear per-tree EVI to campus mean (mean of tree samples per date).
#' Nearby trees may share a 30 m pixel; we document mean-of-trees (not unique pixels).
#' When both L30 and S30 are clear for the same tree-date, average sensors first
#' so each tree contributes once to the campus mean.
hls_evi_aggregate_campus_mean <- function(per_tree, source = "appeears") {
  if (nrow(per_tree) == 0) {
    return(hls_evi_empty_campus_mean())
  }
  per_tree |>
    dplyr::group_by(.data$date, .data$doy, .data$year, .data$individual_id) |>
    dplyr::summarize(
      evi = mean(.data$evi, na.rm = TRUE),
      n_sensor_obs = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$date, .data$doy, .data$year) |>
    dplyr::summarize(
      evi_mean = mean(.data$evi, na.rm = TRUE),
      n_trees = dplyr::n_distinct(.data$individual_id),
      n_clear = sum(.data$n_sensor_obs),
      .groups = "drop"
    ) |>
    dplyr::mutate(source = source) |>
    dplyr::arrange(.data$date)
}

hls_evi_sensor_for_product <- function(product) {
  dplyr::case_when(
    grepl("HLSL30", product, fixed = TRUE) ~ "L30",
    grepl("HLSS30", product, fixed = TRUE) ~ "S30",
    TRUE ~ NA_character_
  )
}

#' Parse AppEEARS point CSV into long per-tree EVI rows.
hls_evi_parse_appeears_csv <- function(path, trees) {
  raw <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  if (nrow(raw) == 0) {
    return(hls_evi_empty_per_tree())
  }

  # AppEEARS typically returns ID / Category from request coordinates.
  id_col <- intersect(c("ID", "id", "Identifier", "Category"), names(raw))
  if (length(id_col) == 0) {
    stop("AppEEARS CSV missing ID column: ", path)
  }
  id_col <- id_col[[1]]

  date_col <- intersect(c("Date", "date", "YYYY-MM-DD"), names(raw))
  if (length(date_col) == 0) {
    stop("AppEEARS CSV missing Date column: ", path)
  }
  date_col <- date_col[[1]]

  lat_col <- intersect(c("Latitude", "latitude", "lat"), names(raw))
  lon_col <- intersect(c("Longitude", "longitude", "lon"), names(raw))

  evi_cols <- names(raw)[grepl("EVI$", names(raw), ignore.case = TRUE) &
    !grepl("QA|quality|bit", names(raw), ignore.case = TRUE)]
  if (length(evi_cols) == 0) {
    evi_cols <- names(raw)[grepl("EVI", names(raw), ignore.case = TRUE)]
  }
  if (length(evi_cols) == 0) {
    message("No EVI columns in ", basename(path), "; skipping.")
    return(hls_evi_empty_per_tree())
  }

  long <- raw |>
    dplyr::select(
      individual_id = dplyr::all_of(id_col),
      date = dplyr::all_of(date_col),
      dplyr::any_of(c(lat_col, lon_col)),
      dplyr::all_of(evi_cols)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(evi_cols),
      names_to = "layer",
      values_to = "evi_raw"
    ) |>
    dplyr::mutate(
      date = as.Date(.data$date),
      individual_id = as.integer(.data$individual_id),
      product = dplyr::case_when(
        grepl("HLSL30", .data$layer, ignore.case = TRUE) ~ hls_evi_products[["L30"]],
        grepl("HLSS30", .data$layer, ignore.case = TRUE) ~ hls_evi_products[["S30"]],
        grepl("L30", .data$layer, ignore.case = TRUE) ~ hls_evi_products[["L30"]],
        grepl("S30", .data$layer, ignore.case = TRUE) ~ hls_evi_products[["S30"]],
        TRUE ~ NA_character_
      ),
      sensor = hls_evi_sensor_for_product(.data$product)
    )

  if (length(lat_col) == 1) {
    long <- dplyr::rename(long, lat = dplyr::all_of(lat_col))
  }
  if (length(lon_col) == 1) {
    long <- dplyr::rename(long, lon = dplyr::all_of(lon_col))
  }

  long <- hls_evi_qc_clear(long)

  # Attach tree coords when CSV lat/lon missing
  if (!("lat" %in% names(long)) || !("lon" %in% names(long))) {
    long <- long |>
      dplyr::left_join(
        trees |> dplyr::select(individual_id, lat, lon),
        by = "individual_id"
      )
  }

  long |>
    dplyr::mutate(
      doy = as.integer(lubridate::yday(.data$date)),
      year = as.integer(lubridate::year(.data$date)),
      source = "appeears"
    ) |>
    dplyr::select(
      date, doy, year, evi, product, sensor, qa_ok,
      individual_id, lat, lon, source
    )
}

hls_evi_submit_point_task <- function(token, trees, start_date, end_date,
                                      task_name, api = hls_evi_api_url) {
  coordinates <- purrr::pmap(
    list(trees$individual_id, trees$lat, trees$lon),
    function(id, lat, lon) {
      list(
        id = as.character(id),
        latitude = lat,
        longitude = lon,
        category = "campus_tree"
      )
    }
  )

  layers <- list(
    list(product = hls_evi_products[["L30"]], layer = "EVI"),
    list(product = hls_evi_products[["S30"]], layer = "EVI")
  )

  task <- list(
    task_type = "point",
    task_name = task_name,
    params = list(
      dates = list(list(
        startDate = format(as.Date(start_date), "%m-%d-%Y"),
        endDate = format(as.Date(end_date), "%m-%d-%Y")
      )),
      layers = layers,
      coordinates = coordinates
    )
  )

  resp <- httr::POST(
    paste0(api, "task"),
    body = task,
    encode = "json",
    hls_evi_auth_header(token),
    httr::content_type_json()
  )
  if (httr::status_code(resp) >= 300) {
    stop(
      "AppEEARS task submit failed (HTTP ", httr::status_code(resp), "): ",
      httr::content(resp, as = "text", encoding = "UTF-8")
    )
  }
  httr::content(resp, as = "parsed", encoding = "UTF-8")$task_id
}

hls_evi_poll_task <- function(token, task_id, api = hls_evi_api_url,
                              poll_seconds = 30, max_hours = 6) {
  deadline <- Sys.time() + max_hours * 3600
  repeat {
    resp <- httr::GET(
      paste0(api, "task/", task_id),
      hls_evi_auth_header(token)
    )
    if (httr::status_code(resp) >= 300) {
      stop(
        "AppEEARS task status failed (HTTP ", httr::status_code(resp), "): ",
        httr::content(resp, as = "text", encoding = "UTF-8")
      )
    }
    status <- httr::content(resp, as = "parsed", encoding = "UTF-8")
    state <- status$status
    message("AppEEARS task ", task_id, " status: ", state)
    if (identical(state, "done")) {
      return(invisible(TRUE))
    }
    if (state %in% c("error", "expired", "deleted")) {
      stop("AppEEARS task ", task_id, " ended with status: ", state)
    }
    if (Sys.time() > deadline) {
      stop("Timed out waiting for AppEEARS task ", task_id)
    }
    Sys.sleep(poll_seconds)
  }
}

hls_evi_download_bundle_csvs <- function(token, task_id, dest_dir,
                                         api = hls_evi_api_url) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  resp <- httr::GET(
    paste0(api, "bundle/", task_id),
    hls_evi_auth_header(token)
  )
  if (httr::status_code(resp) >= 300) {
    stop(
      "AppEEARS bundle list failed (HTTP ", httr::status_code(resp), "): ",
      httr::content(resp, as = "text", encoding = "UTF-8")
    )
  }
  files <- httr::content(resp, as = "parsed", encoding = "UTF-8")$files
  csv_paths <- character()
  for (f in files) {
    file_name <- f$file_name
    if (!grepl("\\.csv$", file_name, ignore.case = TRUE)) {
      next
    }
    # Skip qa/lookup helper CSVs when present; keep main point extracts
    if (grepl("quality|lookup|-qa-", file_name, ignore.case = TRUE) &&
      !grepl("EVI", file_name, ignore.case = TRUE)) {
      next
    }
    out_path <- file.path(dest_dir, basename(file_name))
    dl <- httr::GET(
      paste0(api, "bundle/", task_id, "/", f$file_id),
      hls_evi_auth_header(token),
      httr::write_disk(out_path, overwrite = TRUE)
    )
    if (httr::status_code(dl) >= 300) {
      warning("Failed to download ", file_name)
      next
    }
    csv_paths <- c(csv_paths, out_path)
  }
  csv_paths
}

#' Fetch full-year EVI at all tree points for the given years (AppEEARS only).
hls_evi_fetch_appeears <- function(trees,
                                   years,
                                   work_dir = file.path(tempdir(), "appeears_hls_evi"),
                                   poll_seconds = 30) {
  creds <- hls_evi_credentials()
  if (!hls_evi_has_credentials(creds)) {
    stop("No Earthdata / AppEEARS credentials in environment.")
  }
  token <- hls_evi_login(creds)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  per_tree <- hls_evi_empty_per_tree()
  for (yr in years) {
    start_date <- as.Date(paste0(yr, "-01-01"))
    end_date <- as.Date(paste0(yr, "-12-31"))
    if (end_date > Sys.Date()) {
      end_date <- Sys.Date()
    }
    task_name <- paste0("campus_hls_evi_", yr)
    message("Submitting AppEEARS point task for ", yr, " ...")
    task_id <- hls_evi_submit_point_task(
      token = token,
      trees = trees,
      start_date = start_date,
      end_date = end_date,
      task_name = task_name
    )
    hls_evi_poll_task(token, task_id, poll_seconds = poll_seconds)
    year_dir <- file.path(work_dir, as.character(yr))
    csvs <- hls_evi_download_bundle_csvs(token, task_id, year_dir)
    for (csv in csvs) {
      parsed <- tryCatch(
        hls_evi_parse_appeears_csv(csv, trees),
        error = function(e) {
          message("Parse failed for ", csv, ": ", conditionMessage(e))
          hls_evi_empty_per_tree()
        }
      )
      per_tree <- dplyr::bind_rows(per_tree, parsed)
    }
  }

  per_tree |>
    dplyr::distinct(date, individual_id, product, .keep_all = TRUE) |>
    dplyr::arrange(date, individual_id, product)
}

hls_evi_try_download_release <- function(dest_dir, repo, tag = hls_evi_release_tag) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  assets <- tryCatch(
    piggyback::pb_list(tag = tag, repo = repo),
    error = function(e) {
      message("No hls-evi release assets: ", conditionMessage(e))
      tibble::tibble(file_name = character())
    }
  )
  if (!is.data.frame(assets) || nrow(assets) == 0) {
    return(invisible(character()))
  }
  wanted <- intersect(
    c(hls_evi_mean_basename, hls_evi_tree_basename),
    unique(assets$file_name)
  )
  if (length(wanted) == 0) {
    return(invisible(character()))
  }
  local <- file.path(dest_dir, wanted)
  missing <- wanted[!file.exists(local)]
  if (length(missing) == 0) {
    return(invisible(local))
  }
  tryCatch(
    {
      piggyback::pb_download(
        file = missing,
        tag = tag,
        repo = repo,
        dest = dest_dir
      )
      invisible(file.path(dest_dir, wanted)[file.exists(file.path(dest_dir, wanted))])
    },
    error = function(e) {
      message("hls-evi release download failed: ", conditionMessage(e))
      invisible(character())
    }
  )
}

hls_evi_try_upload_release <- function(paths, repo, tag = hls_evi_release_tag) {
  # Reuse safe helpers when sourced from the targets pipeline.
  ensure_fn <- if (exists("cp_ensure_release", mode = "function")) {
    cp_ensure_release
  } else {
    function(tag, repo) {
      tryCatch(
        piggyback::pb_list(tag = tag, repo = repo),
        error = function(e) {
          piggyback::pb_release_create(
            tag = tag,
            repo = repo,
            name = tag,
            body = paste(
              "Cached HLS EVI (HLSL30_VI + HLSS30_VI) extracts for campus trees.",
              "Campus-mean Parquet is the site chart input.",
              sep = "\n"
            )
          )
        }
      )
      invisible(TRUE)
    }
  }
  upload_fn <- if (exists("safe_pb_upload", mode = "function")) {
    safe_pb_upload
  } else {
    function(file, tag, repo, overwrite = TRUE) {
      tryCatch(
        {
          piggyback::pb_upload(file = file, tag = tag, repo = repo, overwrite = overwrite)
          TRUE
        },
        error = function(e) {
          message("Upload skipped for ", basename(file), ": ", conditionMessage(e))
          FALSE
        }
      )
    }
  }

  ensure_fn(tag = tag, repo = repo)
  for (p in paths) {
    if (file.exists(p)) {
      upload_fn(file = p, tag = tag, repo = repo, overwrite = TRUE)
    }
  }
  invisible(TRUE)
}

#' Ensure local HLS EVI Parquet exists for the site.
#'
#' Priority: AppEEARS fetch (when credentials present) → Release `hls-evi` →
#' committed seed fixture → empty schema Parquet. Never hard-fails the build.
cp_ensure_hls_evi <- function(trees,
                              repo = "AdamWilsonLabEDU/campus_phenology",
                              dest_dir = hls_evi_dir_default,
                              year_start = 2019L,
                              year_stop = lubridate::year(Sys.Date()),
                              refresh_current_year = TRUE,
                              upload = TRUE) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  mean_path <- file.path(dest_dir, hls_evi_mean_basename)
  tree_path <- file.path(dest_dir, hls_evi_tree_basename)
  years <- hls_evi_years(year_start, year_stop)

  fetched <- FALSE
  if (hls_evi_has_credentials()) {
    message("Earthdata credentials found; attempting AppEEARS HLS EVI fetch.")
    tryCatch(
      {
        years_to_fetch <- years
        if (file.exists(mean_path) && isTRUE(refresh_current_year)) {
          existing <- arrow::read_parquet(mean_path)
          have_years <- unique(existing$year)
          current_year <- lubridate::year(Sys.Date())
          years_to_fetch <- setdiff(years, setdiff(have_years, current_year))
          if (current_year %in% years) {
            years_to_fetch <- union(years_to_fetch, current_year)
          }
        }
        if (length(years_to_fetch) > 0) {
          per_tree_new <- hls_evi_fetch_appeears(trees, years_to_fetch)
          if (file.exists(tree_path)) {
            per_tree <- dplyr::bind_rows(
              arrow::read_parquet(tree_path),
              per_tree_new
            ) |>
              dplyr::distinct(date, individual_id, product, .keep_all = TRUE)
          } else {
            per_tree <- per_tree_new
          }
          mean_df <- hls_evi_aggregate_campus_mean(per_tree, source = "appeears")
          hls_evi_write_parquet(per_tree, tree_path)
          hls_evi_write_parquet(mean_df, mean_path)
          fetched <- TRUE
          if (isTRUE(upload)) {
            hls_evi_try_upload_release(c(mean_path, tree_path), repo = repo)
          }
        }
      },
      error = function(e) {
        message("AppEEARS HLS EVI fetch soft-failed: ", conditionMessage(e))
      }
    )
  } else {
    message(
      "No Earthdata credentials (EARTHDATA_USERNAME/PASSWORD or APPEEARS_TOKEN); ",
      "skipping AppEEARS fetch."
    )
  }

  if (!file.exists(mean_path)) {
    message("Trying GitHub Release tag '", hls_evi_release_tag, "' ...")
    hls_evi_try_download_release(dest_dir, repo = repo)
  }

  if (!file.exists(mean_path)) {
    fixture_mean <- file.path(hls_evi_fixture_dir, hls_evi_mean_basename)
    if (!file.exists(fixture_mean)) {
      message("Building committed seed fixture under ", hls_evi_fixture_dir)
      hls_evi_ensure_fixture(hls_evi_fixture_dir)
    }
    if (file.exists(fixture_mean)) {
      message("Using seed fixture for campus-mean HLS EVI.")
      file.copy(fixture_mean, mean_path, overwrite = TRUE)
    }
  }

  if (!file.exists(mean_path)) {
    message("Writing empty HLS EVI campus-mean Parquet so the site can build.")
    hls_evi_write_parquet(hls_evi_empty_campus_mean(), mean_path)
  }

  list(
    campus_mean = normalizePath(mean_path, winslash = "/", mustWork = TRUE),
    per_tree = if (file.exists(tree_path)) {
      normalizePath(tree_path, winslash = "/", mustWork = TRUE)
    } else {
      NA_character_
    },
    fetched = fetched
  )
}

#' Read campus-mean HLS EVI for plotting; empty tibble if missing/unreadable.
cp_read_hls_evi_campus_mean <- function(path = file.path(
                                          hls_evi_dir_default,
                                          hls_evi_mean_basename
                                        )) {
  if (!file.exists(path)) {
    return(hls_evi_empty_campus_mean())
  }
  tryCatch(
    {
      arrow::read_parquet(path) |>
        dplyr::mutate(
          date = as.Date(.data$date),
          doy = as.integer(.data$doy),
          year = as.integer(.data$year),
          evi_mean = as.numeric(.data$evi_mean)
        )
    },
    error = function(e) {
      message("Could not read ", path, ": ", conditionMessage(e))
      hls_evi_empty_campus_mean()
    }
  )
}

# Standalone entry when invoked as `Rscript R/get_hls_evi.R` (not when sourced).
.hls_evi_cmd <- commandArgs(trailingOnly = FALSE)
.hls_evi_file <- sub("^--file=", "", .hls_evi_cmd[startsWith(.hls_evi_cmd, "--file=")])
if (length(.hls_evi_file) == 1L &&
      grepl("get_hls_evi\\.R$", .hls_evi_file) &&
      file.exists("data/trees.csv")) {
  .hls_evi_trees <- readr::read_csv("data/trees.csv", show_col_types = FALSE)
  cp_ensure_hls_evi(.hls_evi_trees)
}
rm(.hls_evi_cmd, .hls_evi_file)
