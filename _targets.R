library(targets)

source("R/0000_setup.R")

tar_option_set(
  packages = c(
    "dplyr",
    "fs",
    "ggfx",
    "ggnewscale",
    "ggpattern",
    "ggplot2",
    "ggspatial",
    "ggthemes",
    "gifski",
    "glue",
    "knitr",
    "lubridate",
    "lwgeom",
    "magick",
    "magrittr",
    "MetBrewer",
    "nngeo",
    "png",
    "purrr",
    "quarto",
    "rmapshaper",
    "sf",
    "shadowtext",
    "smoothr",
    "stringr",
    "terra",
    "tibble",
    "tidyr",
    "tidyterra",
    "tidyverse",
    "units",
    "viridis"
  )
)

target_version <- Sys.getenv("COVERAGE_VERSION", unset = "20260421")

preprocess_scripts <- c(
  "R/1001_preprocess_AB.R",
  "R/1001_preprocess_BC.R",
  "R/1001_preprocess_NB.R",
  "R/1001_preprocess_NS.R",
  "R/1001_preprocess_ON.R",
  "R/1001_preprocess_PEI.R",
  "R/1001_preprocess_QC_v2.R",
  "R/1001_preprocess_SK.R"
)

processing_scripts <- c(
  "R/1100_combineAll_noOverlaps.R",
  "R/1500_combineALL_withOverlaps.R"
)

map_table_scripts <- c(
  "R/2000_maps_setup.R",
  "R/2000_maps_main.R",
  "R/2000_maps_focused.R",
  "R/2000_maps_updateLog.R",
  "R/2000_maps_animation.R",
  "R/3001_coverageManagedUnmanaged.R",
  "R/3001_theTable.R",
  "R/3001_theTable_v2.R",
  "R/3003_coverageMultiTemporal.R",
  "R/3004_acquisition_area_over_time.R"
)

run_scripts_with_env <- function(scripts, env, output_files, input_files = character()) {
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

  target_env <- new.env(parent = globalenv())
  for (script in scripts) {
    message("Running ", script)
    source(script, local = target_env)
  }

  output_files
}

previous_main_coverage_file <- function(current_file) {
  files <- latest_files_by_pattern(
    file.path("layers/ALS_coverage_layer/main", "ALS_coverage_all_*.rds"),
    n = 2,
    stamp_regex = "ALS_coverage_all_(\\d{8})\\.rds",
    label = "main ALS coverage RDS"
  )

  normalized_current <- normalizePath(current_file, winslash = "/", mustWork = FALSE)
  normalized_files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  previous <- files[normalized_files != normalized_current]

  if (length(previous) == 0) {
    stop("Could not identify a previous main coverage file.", call. = FALSE)
  }

  previous[1]
}

preprocessed_outputs <- c(
  coverage_output_paths("AB")$file,
  coverage_output_paths("AB")$diss_file,
  coverage_output_paths("BC")$file,
  coverage_output_paths("BC")$diss_file,
  coverage_output_paths("NB")$file,
  coverage_output_paths("NB")$diss_file,
  coverage_output_paths("NS")$file,
  coverage_output_paths("NS")$diss_file,
  file.path("layers/source_layers/ON", "ALS_ON_Y1_to_Y8_wDensity.gpkg"),
  coverage_output_paths("ON")$file,
  coverage_output_paths("ON")$diss_file,
  coverage_output_paths("PEI")$file,
  coverage_output_paths("PEI")$diss_file,
  coverage_output_paths("QC")$file,
  coverage_output_paths("QC")$diss_file,
  coverage_output_paths("SK")$file,
  coverage_output_paths("SK")$diss_file
)

coverage_main_file <- file.path(
  "layers/ALS_coverage_layer/main",
  glue("ALS_coverage_all_{target_version}.rds")
)
coverage_clipped_file <- "layers/ALS_coverage_all_2025_clipped.rds"
coverage_generalized_file <- file.path(
  "layers/ALS_coverage_layer/generalized",
  glue("ALS_coverage_all_{target_version}_generalized_v2.gpkg")
)
multitemporal_output_file <- file.path(
  "layers/ALS_coverage_layer/multitemporal",
  glue("ALS_coverage_multitemporal_{target_version}.gpkg")
)
overlap_output_file <- file.path(
  "layers/ALS_coverage_layer/overlap",
  glue("ALS_coverage_overlap_{target_version}.gpkg")
)

processing_outputs <- c(
  coverage_main_file,
  coverage_clipped_file,
  coverage_generalized_file,
  multitemporal_output_file,
  overlap_output_file
)

table_outputs <- c(
  "layers/coverageManagedUnmanaged.rds",
  "layers/manage_unmanaged_byJurisdiction.rds",
  "layers/Stats.rds",
  "layers/dataForTheFigure.rds",
  "layers/coverageOverTime.rds",
  "layers/dataForTheFigure_v3.rds",
  "layers/summary_multitemporal_list.rds",
  "layers/dataForTheFigure_v4.rds"
)

map_outputs <- c(
  "img/map0_overview.png",
  "img/map1_ALS_coverage.png",
  "img/map2_ALS_density.png",
  "img/map3_ALS_AcquisitionYear.png",
  "img/map4_ALS_overlap.png",
  "img/map2_ALS_density_focused1.png",
  "img/map3_ALS_AcquisitionYear_focused1.png",
  "img/map1_ALS_coverage_focused1.png",
  "img/map4_ALS_overlap_focus1.png",
  "img/map2_ALS_density_focused2.png",
  "img/map3_ALS_AcquisitionYear_focused2.png",
  "img/map1_ALS_coverage_focused2.png",
  "img/map4_ALS_overlap_focus2.png",
  glue("img/UpdateLog/map_newAcquisitions_{ymd(target_version)}.png"),
  "img/animation_ALS_over_time.gif"
)

qmd_files <- list.files(pattern = "\\.qmd$", full.names = TRUE)
site_outputs <- file.path(
  "docs",
  paste0(tools::file_path_sans_ext(basename(qmd_files)), ".html")
)

list(
  tar_target(workflow_version, target_version),
  tar_target(
    workflow_scripts,
    c(
      "R/0000_setup.R",
      "R/config/packages.R",
      "R/config/paths.R",
      "R/config/reference_tables.R",
      "R/config/theme.R",
      list.files("R/functions", pattern = "\\.R$", full.names = TRUE),
      preprocess_scripts,
      processing_scripts,
      map_table_scripts
    ),
    format = "file"
  ),
  tar_target(qmd_inputs, qmd_files, format = "file"),
  tar_target(
    preprocessing,
    run_scripts_with_env(
      scripts = preprocess_scripts,
      env = c(COVERAGE_VERSION = workflow_version),
      output_files = preprocessed_outputs,
      input_files = workflow_scripts
    ),
    format = "file"
  ),
  tar_target(
    processing,
    run_scripts_with_env(
      scripts = processing_scripts,
      env = c(
        COVERAGE_VERSION = workflow_version,
        COVERAGE_MAIN_FILE = coverage_main_file,
        COVERAGE_CLIPPED_FILE = coverage_clipped_file,
        COVERAGE_GENERALIZED_FILE = coverage_generalized_file,
        MULTITEMPORAL_OUTPUT_FILE = multitemporal_output_file,
        OVERLAP_OUTPUT_FILE = overlap_output_file
      ),
      output_files = processing_outputs,
      input_files = preprocessing
    ),
    format = "file"
  ),
  tar_target(
    previous_main_coverage,
    previous_main_coverage_file(coverage_main_file),
    format = "file"
  ),
  tar_target(
    maps_and_tables,
    run_scripts_with_env(
      scripts = map_table_scripts,
      env = c(
        COVERAGE_VERSION = workflow_version,
        COVERAGE_MAIN_FILE = coverage_main_file,
        COVERAGE_GENERALIZED_FILE = coverage_generalized_file,
        MULTITEMPORAL_OUTPUT_FILE = multitemporal_output_file,
        OVERLAP_OUTPUT_FILE = overlap_output_file,
        UPDATE_LOG_CURRENT_FILE = coverage_main_file,
        UPDATE_LOG_PREVIOUS_FILE = previous_main_coverage
      ),
      output_files = c(map_outputs, table_outputs),
      input_files = c(processing, previous_main_coverage)
    ),
    format = "file"
  ),
  tar_target(
    website,
    {
      invisible(maps_and_tables)
      purrr::walk(qmd_inputs, quarto::quarto_render)
      site_outputs
    },
    format = "file"
  )
)
