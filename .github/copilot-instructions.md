## AI Coding Agent Instructions for Campus Phenology

**Project Overview**
- **Site Type:** Quarto website rendered to `_site/`; do not edit `_site/` directly. Core config in [_quarto.yml](_quarto.yml).
- **Data Flow:** Pre-render R pipeline pulls USA-NPN data, caches semester CSV/Parquet via GitHub Releases, writes local Parquet to [data/processed](data/processed), then Quarto pages read Parquet at render time.
- **Generated Pages:** Semester and per-student HTML pages written to [generated/](generated/) using templates in [template/](template/). Index and other QMDs live at repo root.

**Key Components**
- **Pre-render hook:** [_quarto.yml](_quarto.yml) runs [scripts/npn_download_prep.R](scripts/npn_download_prep.R) before every render. It:
  - Pulls status observations via `rnpn`, computes `semester` using `lubridate::semester(with_year=TRUE)`.
  - Uses `piggyback` release assets as the authoritative store; always refreshes current semester; backfills missing historical semesters.
  - Persists global data: [data/processed/full_data.parquet](data/processed/full_data.parquet), [trees.parquet](data/processed/trees.parquet), [weekly_observer_stats.parquet](data/processed/weekly_observer_stats.parquet), [semester_observer_stats.parquet](data/processed/semester_observer_stats.parquet).
  - Generates per-semester and per-student QMD files in [generated/](generated/) from [template/semester_template.qmd](template/semester_template.qmd) and [template/student_template.qmd](template/student_template.qmd). Each generated QMD has an R chunk that loads filtered Parquet data from hidden files (e.g., `.semester_2025.2_ds.parquet`) into standalone variables (`ds`, `ds_obs`, `ds_obs_weekly`) using `arrow::read_parquet()`.
  - Overwrites [semesters.qmd](semesters.qmd) with an auto-generated semester index table.
- **Front-end QMDs:**
  - [index.qmd](index.qmd) reads Arrow Parquet from `data/processed` and builds Leaflet/DT/ggplot outputs.
  - [semesters.qmd](semesters.qmd) is auto-generated. Don’t hand-edit; change generation in [scripts/npn_download_prep.R](scripts/npn_download_prep.R).
  - [include/after_body.html](include/after_body.html) injects footer markup.
- **Optional data task:** [R/get_modis.R](R/get_modis.R) fetches MODIS NDVI per semester and uploads per-semester assets as release tags. Not wired into site render by default.

**Build & Run**
- **Local preview:**
  ```bash
  # Installs Quarto separately; R deps via renv
  quarto preview
  ```
  The pre-render will run first and may take several minutes if semesters are missing.
- **Render once:**
  ```bash
  quarto render
  ```
- **Run only the data pipeline:**
  ```bash
  Rscript scripts/npn_download_prep.R
  ```
- **CI publish:** [\.github/workflows/publish_github_actions.yml](.github/workflows/publish_github_actions.yml) renders with Quarto and deploys to `gh-pages`. Dependencies are pinned via [renv.lock](renv.lock) (R 4.5.2).

**Conventions & Patterns**
- **Semester IDs:** Strings like `YYYY.1` (Spring) or `YYYY.2` (Fall) from `lubridate::semester()`.
- **Generated filenames:**
  - Semester pages: `generated/semester_YYYY.S.qmd` → rendered to HTML.
  - Student pages: `generated/student_YYYY.S_<NNID>.qmd` → rendered to HTML.
  - Hidden data files: `generated/.semester_YYYY.S_ds.parquet`, `generated/.student_YYYY.S_<NNID>_ds.parquet`, etc. (loaded at render time).
- **Data loading pattern:** Generated QMD files include an R chunk that loads Parquet data into standalone variables (`ds`, `ds_obs`, `ds_obs_weekly`) via `arrow::read_parquet()`. Templates reference these variables directly, not through `params$`. The `params` object only holds scalar metadata (semester ID, NNID, file paths, thresholds).
- **Source of truth:** Semester CSV/Parquet stored as GitHub Release assets (`npn_obs_network-891_semester-YYYY.S.{csv,parquet}`) managed by `piggyback`.
- **Do not edit:** `_site/` artifacts, auto-generated [semesters.qmd](semesters.qmd), or any files under [generated/](generated/). Update templates or the pre-render script instead.

**External Dependencies**
- R packages used across the site: `tidyverse`, `lubridate`, `arrow`, `leaflet`, `DT`, `zoo`, `sf`, `cowplot`, `rmarkdown`; data pipeline adds `rnpn`, `piggyback`, `glue`.
- Auth for uploads: set `GITHUB_TOKEN` in env to allow `piggyback` to create/update release assets; downloads of public assets typically work without a token.

**Common Tasks**
- **Add a new visualization to semester pages:** Modify [template/semester_template.qmd](template/semester_template.qmd); run `Rscript scripts/npn_download_prep.R` to regenerate files, then `quarto render`. Do not edit files under [generated/](generated/) directly. Use `ds`, `ds_obs`, and `ds_obs_weekly` variables in your code (these are loaded via `arrow::read_parquet()` in the generated files).
- **Change site nav or theme:** Edit [_quarto.yml](_quarto.yml) (e.g., navbar, theme, `includes.after_body`).
- **Refresh only MODIS NDVI:** Run `Rscript R/get_modis.R` (uploads per-semester Parquet to release tags). Integrate into pages by reading from [data/modis_semesters](data/modis_semesters) or via `piggyback` download.

**Debugging Tips**
- **Missing Parquet in pages:** Ensure [scripts/npn_download_prep.R](scripts/npn_download_prep.R) completed; check for files under [data/processed](data/processed) and [generated/](generated/) (look for hidden `.semester_*.parquet` files).
- **Params binding errors:** Never try to assign to `params$` in generated files. The script creates standalone variables (`ds`, `ds_obs`, `ds_obs_weekly`) loaded via `arrow::read_parquet()`. Templates should reference these variables directly.
- **Release upload errors:** Confirm `GITHUB_TOKEN` is present; verify repo/tag in the script match [AdamWilsonLabEDU/campus_phenology](https://github.com/AdamWilsonLabEDU/campus_phenology).
- **Slow first run:** The pre-render backfills all missing semesters; subsequent runs reuse release caches.
