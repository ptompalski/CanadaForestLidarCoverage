library(targets)

source("R/0000_setup.R")

tar_option_set(
  packages = c(
    "dplyr",
    "fs",
    "glue",
    "lwgeom",
    "magrittr",
    "nngeo",
    "purrr",
    "rmapshaper",
    "sf",
    "stringr",
    "terra",
    "tibble",
    "tidyr",
    "tidyverse",
    "units"
  )
)

target_output_dir <- "scratch/targets-example"
target_version <- format(Sys.Date(), "%Y%m%d")

run_script_with_env <- function(script, env, output_files, input_files = character()) {
  invisible(input_files)

  old_values <- Sys.getenv(names(env), unset = NA_character_)
  names(old_values) <- names(env)

  on.exit({
    for (env_name in names(old_values)) {
      if (is.na(old_values[[env_name]])) {
        Sys.unsetenv(env_name)
      } else {
        do.call(Sys.setenv, as.list(setNames(old_values[[env_name]], env_name)))
      }
    }
  }, add = TRUE)

  do.call(Sys.setenv, as.list(env))
  source(script, local = new.env(parent = globalenv()))

  output_files
}

tar_pipeline(
  tar_target(
    preprocessed_coverage_files,
    c(
      preprocessed_coverage_path("AB", dissolved = TRUE),
      preprocessed_coverage_path("BC", dissolved = TRUE),
      preprocessed_coverage_path("NB", dissolved = TRUE),
      preprocessed_coverage_path("NS", dissolved = TRUE),
      preprocessed_coverage_path("ON", dissolved = TRUE),
      preprocessed_coverage_path("PEI", dissolved = TRUE),
      preprocessed_coverage_path("QC", dissolved = TRUE),
      preprocessed_coverage_path("SK", dissolved = TRUE)
    ),
    format = "file"
  ),
  tar_target(
    combined_coverage_outputs,
    run_script_with_env(
      script = "R/1100_combineAll_noOverlaps.R",
      env = c(
        COVERAGE_MAIN_FILE = file.path(
          target_output_dir,
          "ALS_coverage_layer/main",
          glue("ALS_coverage_all_{target_version}.rds")
        ),
        COVERAGE_CLIPPED_FILE = file.path(
          target_output_dir,
          "ALS_coverage_all_2025_clipped.rds"
        ),
        COVERAGE_GENERALIZED_FILE = file.path(
          target_output_dir,
          "ALS_coverage_layer/generalized",
          glue("ALS_coverage_all_{target_version}_generalized_v2.gpkg")
        )
      ),
      output_files = c(
        file.path(
          target_output_dir,
          "ALS_coverage_layer/main",
          glue("ALS_coverage_all_{target_version}.rds")
        ),
        file.path(
          target_output_dir,
          "ALS_coverage_all_2025_clipped.rds"
        ),
        file.path(
          target_output_dir,
          "ALS_coverage_layer/generalized",
          glue("ALS_coverage_all_{target_version}_generalized_v2.gpkg")
        )
      ),
      input_files = preprocessed_coverage_files
    ),
    format = "file"
  )
)
