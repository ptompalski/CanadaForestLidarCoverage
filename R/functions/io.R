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
