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
coverage_version <- Sys.getenv("COVERAGE_VERSION", unset = "20260421")
Sys.setenv(COVERAGE_VERSION = coverage_version)
targets::tar_make()
