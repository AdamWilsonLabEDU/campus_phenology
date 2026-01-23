#!/usr/bin/env Rscript

# Quarto pre-render entrypoint: runs the targets pipeline and ensures
# all file outputs needed for rendering exist.

if (!requireNamespace("targets", quietly = TRUE)) {
  stop(
    "The 'targets' package is required. ",
    "Run renv::restore() or install targets in this project."
  )
}

targets::tar_make(
  names = "site_ready",
  callr_function = NULL
)
