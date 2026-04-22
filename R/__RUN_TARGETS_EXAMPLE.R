# Run the targets example pipeline.
#
# The pipeline itself is defined in _targets.R. This script is only a thin
# wrapper so it can be sourced from RStudio in the same style as __RUN_UPDATES.R.

if (!requireNamespace("targets", quietly = TRUE)) {
  stop(
    "The targets package is not installed. Install it with install.packages('targets').",
    call. = FALSE
  )
}

targets::tar_make()
