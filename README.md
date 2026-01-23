# UB Campus Phenology Website

Quarto website that ingests USA–NPN status observations for a campus phenology project, caches semester-partitioned datasets in a GitHub Release, and renders overview pages plus per-semester and per-student summaries.

## How it works
- Quarto pre-render hook runs an R pipeline before every render (see [_quarto.yml](_quarto.yml)).
- The pipeline in [scripts/npn_download_prep.R](scripts/npn_download_prep.R):
	- Downloads USA–NPN status observations via `rnpn`.
	- Downloads existing semester-partitioned release assets (GitHub Release tag `npn-data`) via `piggyback` and refreshes the current semester.
	- Writes core Parquet files to [data/processed](data/processed).
	- Writes per-semester CSV exports to `data/cache/csv/` (raw observations + per-observer summary).
	- Regenerates [semesters.qmd](semesters.qmd) and generates per-semester/per-student/per-tree `.qmd` files under [generated/](generated/) from templates.
	- Attempts to upload new/updated assets to the GitHub Release; if auth/permissions are missing, uploads are skipped and the pipeline continues.
- Frontend pages (e.g., [index.qmd](index.qmd)) read the Parquet files (Arrow) at render time; Quarto renders `.qmd` to HTML under [_site](_site/). Do not edit `_site/` directly.

Key files/directories:
- Config: [_quarto.yml](_quarto.yml), [.github/workflows/publish_github_actions.yml](.github/workflows/publish_github_actions.yml)
- Pipeline: [scripts/npn_download_prep.R](scripts/npn_download_prep.R)
- Templates: [template/semester_template.qmd](template/semester_template.qmd), [template/student_template.qmd](template/student_template.qmd)
- Data outputs: [data/processed/full_data.parquet](data/processed/full_data.parquet), [data/processed/trees.parquet](data/processed/trees.parquet), [data/processed/weekly_observer_stats.parquet](data/processed/weekly_observer_stats.parquet), [data/processed/semester_observer_stats.parquet](data/processed/semester_observer_stats.parquet)
- Per-semester CSV exports (written locally to `data/cache/csv/` and expected as Release assets):
	- `npn_obs_network-<NETWORKID>_semester-YYYY.S.csv` (raw observations)
	- `npn_obs_network-<NETWORKID>_semester-YYYY.S_observer_summary.csv` (one row per NNID)
- Optional NDVI task: [R/get_modis.R](R/get_modis.R) writes to [data/modis_semesters](data/modis_semesters)

## Run locally
Prerequisites: Quarto CLI (https://quarto.org/docs/get-started/), R (pinned via `renv.lock`, R 4.5.2). Optionally set `GITHUB_TOKEN` if you want to upload release assets when running the pipeline locally.

```bash
# Restore R packages (inside R, or from shell)
R -q -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore()"

# Preview site (runs pre-render pipeline first)
quarto preview

# Render once
quarto render

# Run only the data pipeline
Rscript scripts/npn_download_prep.R
```

### Clean re-seed (safe reset for migration/testing)
The pipeline will re-generate outputs. To start fresh locally without touching source files or committed data like `data/trees.csv`, you can remove caches and derived artifacts:

```bash
rm -rf data/cache
rm -rf generated
rm -rf _site
rm -f data/processed/*.parquet
```
Then run:

```bash
Rscript scripts/npn_download_prep.R
quarto render
```

## Use this repo for a new NPN campus project
Update the following to target your project’s USA–NPN network/project ID and branding.

1) Set your NPN network ID in the pipeline:
- [scripts/npn_download_prep.R](scripts/npn_download_prep.R)

Example change:

```r
network_id <- 1234   # replace 1234 with your NPN network id
```

2) Point to your GitHub repository (used by `piggyback` for release assets):

```r
repo <- "YourOrg/your_repo_name"
release_tag <- "npn-data"  # keep or change, but use consistently
```

3) Replace campus tree metadata with your site’s trees:
- Edit [data/trees.csv](data/trees.csv) with columns: `lat, lon, tag, species, common_name, individual_id`.
- `individual_id` must match USA–NPN individual identifiers so joins and maps work.

4) Optional scope tweaks:
- `year_start` in the scripts to control historical backfill.
- Title/branding in [_quarto.yml](_quarto.yml) and footer in [include/after_body.html](include/after_body.html).

5) Prime and render:

```bash
Rscript scripts/npn_download_prep.R   # downloads data, attempts Release uploads, generates pages
quarto render                         # builds the site to _site/
```

## Continuous deployment
GitHub Actions workflow [publish_github_actions.yml](.github/workflows/publish_github_actions.yml) sets up Quarto, R + `renv`, installs needed Linux libs (e.g., for `sf`/`arrow`), runs the pre-render pipeline, and publishes to `gh-pages`. It uses the repository `GITHUB_TOKEN` secret automatically.

## Notes and conventions
- Semester identifiers follow `YYYY.1` (Spring) and `YYYY.2` (Fall).
- Release assets follow:
	- `npn_obs_network-<NETWORKID>_semester-YYYY.S.parquet` (semester partition)
	- `npn_obs_network-<NETWORKID>_semester-YYYY.S.csv` (raw observations)
	- `npn_obs_network-<NETWORKID>_semester-YYYY.S_observer_summary.csv` (one row per NNID)
- Pages are generated under [generated/](generated/) and linked from [semesters.qmd](semesters.qmd); do not hand-edit generated files.
- The student “grade” shown on student pages is `obs_week_percent`: mean weekly percent-of-requirement, capped at 100.

## Troubleshooting
- Missing Parquet on render: ensure the pre-render finished and files are in [data/processed](data/processed).
- Release upload failures locally: set `GITHUB_TOKEN` with permission to create/upload Release assets in this repo; otherwise uploads are skipped.
- Slow first run: backfills all missing semesters; subsequent runs reuse the release cache.
