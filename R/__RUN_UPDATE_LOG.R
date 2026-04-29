if (!requireNamespace("targets", quietly = TRUE)) {
  stop(
    "The targets package is not installed. Install it with install.packages('targets').",
    call. = FALSE
  )
}

source("R/0000_setup.R")

current_version <- Sys.getenv("UPDATE_LOG_CURRENT_VERSION", unset = "")
previous_version <- Sys.getenv("UPDATE_LOG_PREVIOUS_VERSION", unset = "")

if (current_version == "" || previous_version == "") {
  stop(
    paste(
      "Set UPDATE_LOG_CURRENT_VERSION and UPDATE_LOG_PREVIOUS_VERSION",
      "before sourcing this script."
    ),
    call. = FALSE
  )
}

current_file <- file.path(
  "layers/ALS_coverage_layer/main",
  glue::glue("ALS_coverage_all_{current_version}.rds")
)
previous_file <- file.path(
  "layers/ALS_coverage_layer/main",
  glue::glue("ALS_coverage_all_{previous_version}.rds")
)

if (!file.exists(current_file)) {
  stop("Current coverage file not found: ", current_file, call. = FALSE)
}

if (!file.exists(previous_file)) {
  stop("Previous coverage file not found: ", previous_file, call. = FALSE)
}

Sys.setenv(
  UPDATE_LOG_CURRENT_FILE = current_file,
  UPDATE_LOG_PREVIOUS_FILE = previous_file
)

source("R/2000_maps_setup.R")
source("R/2000_maps_updateLog.R")
