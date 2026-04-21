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
