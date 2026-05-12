# Run the targets pipeline.
#
# The pipeline itself is defined in _targets.R. This script is only a thin
# wrapper so it can be sourced from RStudio.

if (!requireNamespace("targets", quietly = TRUE)) {
  stop(
    "The targets package is not installed. Install it with install.packages('targets').",
    call. = FALSE
  )
}

# Edit this value when producing a new dated coverage release, or override it
# before sourcing this script with Sys.setenv(COVERAGE_VERSION = "YYYYMMDD").
# coverage_version <- "20260429"
coverage_version <- Sys.getenv(
  "COVERAGE_VERSION",
  unset = format(Sys.Date(), "%Y%m%d")
)
Sys.setenv(COVERAGE_VERSION = coverage_version)

# Set TARGETS_WORKERS before sourcing this script to control the number of
# concurrent local worker processes. The default is conservative because the
# geospatial stages can be memory intensive on Windows.
targets_workers <- 8 #Sys.getenv("TARGETS_WORKERS", unset = "8")
Sys.setenv(TARGETS_WORKERS = targets_workers)

workers_n <- suppressWarnings(as.integer(targets_workers))
if (is.na(workers_n) || workers_n < 1L) {
  workers_n <- 1L
}

if (workers_n > 1L) {
  message("Running targets with ", workers_n, " local workers.")
} else {
  message("Running targets sequentially with 1 local worker.")
}

targets::tar_make()
