# targets pipeline for the full Canada forest lidar coverage update.
#
# Run with source("R/__RUN_TARGETS.R") or targets::tar_make().
# Set COVERAGE_VERSION first when producing a new dated release, e.g.
# Sys.setenv(COVERAGE_VERSION = "20260421").

suppressPackageStartupMessages({
  library(targets)
  source("R/0000_setup.R")
})

# R/0000_setup.R loads the project package stack. Keeping packages empty here
# prevents targets from re-attaching packages and printing startup messages.
tar_option_set(packages = character())

# Explicit dated version used in output filenames. Keeping this parameter stable
# is what lets targets skip completed stages on later runs.
target_version <- Sys.getenv("COVERAGE_VERSION", unset = "20260421")

# Existing project scripts are still the unit of work. The targets below group
# them into stages so we can refactor individual scripts into functions later.
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

# GeoNB publishes the New Brunswick lidar index as a ZIP download. The HEAD
# request is cheap and lets us detect remote changes without downloading the ZIP.
nb_lidar_index_url <- "https://geonb.snb.ca/downloads/lidar_index/geonb_li-idl_shp.zip"
nb_lidar_index_dir <- "layers/source_layers/NB"
nb_lidar_index_metadata_file <- file.path(
  nb_lidar_index_dir,
  "geonb_li-idl_shp_headers.json"
)
nb_lidar_index_files <- c(
  cgvd2013 = file.path(nb_lidar_index_dir, "geonb_li_idl_cgvd2013.shp"),
  cgvd1928 = file.path(nb_lidar_index_dir, "geonb_li_idl_cgvd1928.shp")
)

remote_file_metadata <- function(url) {
  response <- curl::curl_fetch_memory(
    url,
    handle = curl::new_handle(nobody = TRUE, followlocation = TRUE)
  )
  headers <- curl::parse_headers_list(response$headers)

  list(
    url = url,
    status_code = response$status_code,
    etag = unname(headers[["etag"]]),
    last_modified = unname(headers[["last-modified"]]),
    content_length = unname(headers[["content-length"]])
  )
}

same_remote_metadata <- function(x, y) {
  fields <- c("url", "etag", "last_modified", "content_length")
  identical(x[fields], y[fields])
}

download_zip_if_changed <- function(url, metadata, dest_dir, metadata_file, output_files) {
  dir_create(dest_dir)
  existing_metadata <- NULL

  if (file.exists(metadata_file)) {
    existing_metadata <- jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  }

  outputs_exist <- all(file.exists(output_files))
  metadata_unchanged <- !is.null(existing_metadata) &&
    same_remote_metadata(metadata, existing_metadata)

  if (!outputs_exist || !metadata_unchanged) {
    temp_zip <- tempfile(fileext = ".zip")
    on.exit(unlink(temp_zip), add = TRUE)

    download.file(url, temp_zip, mode = "wb", quiet = TRUE)
    utils::unzip(temp_zip, exdir = dest_dir, overwrite = TRUE)
    jsonlite::write_json(metadata, metadata_file, auto_unbox = TRUE, pretty = TRUE)
  }

  output_files
}

# Source a set of legacy scripts inside one target while temporarily setting
# environment variables for versioned inputs/outputs. Messages are suppressed to
# keep the targets log readable, but warnings and errors are still shown.
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
    suppressPackageStartupMessages(
      suppressMessages(source(script, local = target_env))
    )
  }

  output_files
}

# The update-log map compares the current main coverage layer to the previous
# dated coverage layer. This helper finds the previous layer without relying on
# "latest file" logic inside the map script.
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

# Files created by the preprocessing stage. These paths are declared explicitly
# so targets can verify that the stage produced what downstream stages need.
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

# Versioned outputs created by the two processing scripts.
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

# Table/statistic outputs consumed by the Quarto pages.
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

# Static map outputs and the animation produced by the map scripts.
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

# Quarto pages and their rendered HTML outputs.
qmd_files <- list.files(pattern = "\\.qmd$", full.names = TRUE)
site_outputs <- file.path(
  "docs",
  paste0(tools::file_path_sans_ext(basename(qmd_files)), ".html")
)

list(
  # Lightweight parameter target. Changing COVERAGE_VERSION invalidates the
  # downstream stages that use dated output paths.
  tar_target(workflow_version, target_version),

  # Track code files so changes to scripts/helpers invalidate affected stages.
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

  # Track website source files separately from generated HTML.
  tar_target(qmd_inputs, qmd_files, format = "file"),

  # Online New Brunswick source check. The metadata target reads only HTTP
  # headers; the file target downloads/unzips only when those headers change.
  tar_target(
    nb_lidar_index_remote_metadata,
    remote_file_metadata(nb_lidar_index_url),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    nb_lidar_index_source_files,
    download_zip_if_changed(
      url = nb_lidar_index_url,
      metadata = nb_lidar_index_remote_metadata,
      dest_dir = nb_lidar_index_dir,
      metadata_file = nb_lidar_index_metadata_file,
      output_files = nb_lidar_index_files
    ),
    format = "file"
  ),

  # Jurisdiction preprocessing. This creates layers/pre-processed/* outputs.
  tar_target(
    preprocessing,
    run_scripts_with_env(
      scripts = preprocess_scripts,
      env = c(
        COVERAGE_VERSION = workflow_version,
        NB_LIDAR_INDEX_CGVD2013_FILE = nb_lidar_index_source_files[["cgvd2013"]],
        NB_LIDAR_INDEX_CGVD1928_FILE = nb_lidar_index_source_files[["cgvd1928"]]
      ),
      output_files = preprocessed_outputs,
      input_files = c(workflow_scripts, nb_lidar_index_source_files)
    ),
    format = "file"
  ),

  # National coverage products: no-overlap coverage, generalized coverage,
  # multitemporal acquisitions, and overlap areas.
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

  # Previous coverage layer used only for the update-log map.
  tar_target(
    previous_main_coverage,
    previous_main_coverage_file(coverage_main_file),
    format = "file"
  ),

  # Maps, animation, and RDS tables/statistics used by the website.
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

  # Render the Quarto website after the image/table targets are current.
  tar_target(
    website,
    {
      invisible(maps_and_tables)
      purrr::walk(qmd_inputs, ~ suppressMessages(
        quarto::quarto_render(.x, quiet = TRUE)
      ))
      site_outputs
    },
    format = "file"
  )
)
