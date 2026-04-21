dissolve_coverage <- function(x, group_cols = c("YEAR", "PPM"), crs = the_crs, make_valid = FALSE) {
  missing_cols <- setdiff(group_cols, names(x))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required coverage column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (make_valid) {
    x <- st_make_valid(x)
  }

  x %>%
    st_zm() %>%
    st_transform(crs = crs) %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(geometry = st_union(geometry), .groups = "drop")
}

clean_coverage_polygons <- function(x) {
  x %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON", warn = FALSE) %>%
    st_as_sf()
}

finalize_available_coverage <- function(x, province) {
  x %>%
    st_make_valid() %>%
    st_as_sf() %>%
    mutate(area = st_area(geometry)) %>%
    mutate(Province = province) %>%
    mutate(isAvailable = 1) %>%
    select(Province, YEAR, PPM, area, isAvailable) %>%
    st_as_sf()
}
