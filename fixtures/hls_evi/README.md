# HLS EVI seed fixture

Committed **placeholder** campus-mean EVI used when:

1. No Earthdata / AppEEARS credentials are available, and
2. The GitHub Release tag `hls-evi` has no assets yet.

| File | Role |
| --- | --- |
| `hls_evi_campus_mean.parquet` | Synthetic seasonal EVI (`source = "seed"`) for years 2023–2024 so `index.qmd` can render the dual-axis section offline |
| `hls_evi_campus_mean.csv` | Same seed as CSV (human-readable backup; pipeline can rebuild Parquet from it) |

**Not real satellite data.** Replace by running `R/get_hls_evi.R` / the `hls_evi` targets step with `EARTHDATA_USERNAME` + `EARTHDATA_PASSWORD` (or `APPEEARS_TOKEN`). Successful fetches upload Parquet to Release tag `hls-evi`; the site then prefers that cache over this seed.

Schema: `date, doy, year, evi_mean, n_trees, n_clear, source`.
