latest_file_by_pattern <- function(pattern, stamp_regex = NULL, label = pattern) {
  files <- Sys.glob(pattern)

  if (length(files) == 0) {
    stop("No files found for ", label, ": ", pattern, call. = FALSE)
  }

  if (is.null(stamp_regex)) {
    file_info <- file.info(files)
    return(files[which.max(file_info$mtime)])
  }

  file_stamps <- tibble(
    file = files,
    stamp = str_match(basename(files), stamp_regex)[, 2]
  ) %>%
    filter(!is.na(stamp)) %>%
    arrange(stamp)

  if (nrow(file_stamps) == 0) {
    stop("No files found with a parseable date stamp for ", label, call. = FALSE)
  }

  file_stamps$file[nrow(file_stamps)]
}

latest_files_by_pattern <- function(pattern, n = 1, stamp_regex = NULL, label = pattern) {
  files <- Sys.glob(pattern)

  if (length(files) < n) {
    stop(
      "Expected at least ",
      n,
      " files for ",
      label,
      ": ",
      pattern,
      call. = FALSE
    )
  }

  if (is.null(stamp_regex)) {
    file_info <- file.info(files)
    return(files[order(file_info$mtime, decreasing = TRUE)][seq_len(n)])
  }

  file_stamps <- tibble(
    file = files,
    stamp = str_match(basename(files), stamp_regex)[, 2]
  ) %>%
    filter(!is.na(stamp)) %>%
    arrange(desc(stamp))

  if (nrow(file_stamps) < n) {
    stop(
      "Expected at least ",
      n,
      " files with a parseable date stamp for ",
      label,
      call. = FALSE
    )
  }

  file_stamps$file[seq_len(n)]
}

coverage_output_paths <- function(
  jurisdiction,
  output_dir = file.path("layers/pre-processed", jurisdiction),
  file_prefix = paste0("ALS_", jurisdiction),
  env_prefix = jurisdiction
) {
  env_prefix <- toupper(env_prefix)
  output_dir <- Sys.getenv(
    paste0(env_prefix, "_OUTPUT_DIR"),
    unset = output_dir
  )
  output_file <- Sys.getenv(
    paste0(env_prefix, "_OUTPUT_FILE"),
    unset = file.path(output_dir, paste0(file_prefix, ".gpkg"))
  )
  output_diss_file <- Sys.getenv(
    paste0(env_prefix, "_OUTPUT_DISS_FILE"),
    unset = file.path(output_dir, paste0(file_prefix, "_diss.gpkg"))
  )

  dir_create(dirname(output_file))
  dir_create(dirname(output_diss_file))

  list(
    dir = output_dir,
    file = output_file,
    diss_file = output_diss_file
  )
}

source_scripts <- function(files) {
  for (file in files) {
    message("Running ", file)
    start_time <- Sys.time()

    source(file)

    elapsed <- round(
      as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      1
    )
    message("Finished ", file, " in ", elapsed, " sec")
  }
}

preprocessed_coverage_path <- function(jurisdiction, dissolved = FALSE) {
  suffix <- if (dissolved) "_diss" else ""

  file.path(
    "layers/pre-processed",
    jurisdiction,
    paste0("ALS_", jurisdiction, suffix, ".gpkg")
  )
}

read_preprocessed_coverage <- function(jurisdictions, dissolved = FALSE, path_overrides = NULL) {
  layers <- map(
    jurisdictions,
    function(jurisdiction) {
      path <- NULL

      if (!is.null(path_overrides) && jurisdiction %in% names(path_overrides)) {
        path <- unname(path_overrides[[jurisdiction]])
      }

      if (is.null(path)) {
        path <- preprocessed_coverage_path(jurisdiction, dissolved = dissolved)
      }

      st_read(path, quiet = TRUE)
    }
  )

  names(layers) <- jurisdictions
  layers
}

save_map_with_logo <- function(
  plot,
  filename,
  width,
  height,
  dpi = 300,
  units = "in",
  logo_image = logo,
  logo_geometry = "500x",
  logo_filter = NULL,
  logo_gravity = "SouthWest",
  logo_offset = "+30+50",
  ...
) {
  ggsave(
    plot = plot,
    filename = filename,
    width = width,
    height = height,
    dpi = dpi,
    units = units,
    ...
  )

  background <- magick::image_read(filename)
  logo_resized <- if (is.null(logo_filter)) {
    magick::image_resize(logo_image, geometry = logo_geometry)
  } else {
    magick::image_resize(logo_image, geometry = logo_geometry, filter = logo_filter)
  }
  output <- magick::image_composite(
    background,
    logo_resized,
    gravity = logo_gravity,
    offset = logo_offset
  )
  magick::image_write(output, filename)

  invisible(filename)
}
