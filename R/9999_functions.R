#' Remove overlaps within a polygon layer by prioritizing one attribute order
#'
#' @param x       An sf object with polygonal geometry.
#' @param attr    Column used to set priority (e.g., "YEAR").
#' @param keep    Which to prioritize: "newest" (higher values first) or "oldest" (lower values first).
#' @param make_valid  If TRUE, run st_make_valid(x) before processing.
#' @param quiet   If FALSE, prints simple progress.
#'
#' @return An sf object with overlaps removed according to the chosen priority.
#' @examples
#' # Keep the most recent year in overlaps:
#' ALS_ON_clean <- remove_overlaps_by_attr(ALS_ON, attr = "YEAR", keep = "newest")
#'
#' # Keep the oldest year in overlaps:
#' ALS_ON_clean_oldest <- remove_overlaps_by_attr(ALS_ON, attr = "YEAR", keep = "oldest")
#'
#'
remove_overlaps_by_attr <- function(
  x,
  attr,
  keep = c("newest", "oldest"),
  make_valid = TRUE,
  quiet = FALSE,
  set_precision = NA_real_
) {
  stopifnot(inherits(x, "sf"))
  stopifnot(is.character(attr), length(attr) == 1, attr %in% names(x))
  keep <- match.arg(keep)

  if (make_valid) {
    if (!quiet) {
      message("Making geometries valid (sf::st_make_valid)...")
    }
    x <- sf::st_make_valid(x)
  }

  # Optional fixed precision to reduce slivers (e.g., set_precision = 0.001 for meters)
  if (!is.na(set_precision)) {
    sf::st_precision(x) <- set_precision
  }

  # Order: features processed earlier "win" in overlaps
  x <- if (keep == "newest") {
    dplyr::arrange(x, dplyr::desc(.data[[attr]]))
  } else {
    dplyr::arrange(x, .data[[attr]])
  }

  out <- x[0, ]
  u <- NULL
  n <- nrow(x)

  for (i in seq_len(n)) {
    current <- x[i, , drop = FALSE]

    if (!is.null(u)) {
      # Compute the difference first, then assign via st_geometry<- (name-agnostic)
      diff_geom <- try(
        sf::st_difference(sf::st_geometry(current), u),
        silent = TRUE
      )

      if (inherits(diff_geom, "try-error")) {
        if (!quiet) {
          message(sprintf("[%d/%d] st_difference failed; skipping", i, n))
        }
        next
      }

      # If the result has zero features (can happen in edge cases), skip safely
      if (length(diff_geom) == 0L) {
        if (!quiet) {
          message(sprintf("[%d/%d] fully overlapped -> skipped", i, n))
        }
        next
      }

      # Assign geometry using the setter so it works no matter the geometry column name
      sf::st_geometry(current) <- diff_geom

      # Drop empties that may result from the difference
      current <- current[!sf::st_is_empty(current), , drop = FALSE]
      if (nrow(current) == 0L) {
        if (!quiet) {
          message(sprintf("[%d/%d] fully overlapped -> skipped", i, n))
        }
        next
      }
    }

    # Append and update the running union
    out <- dplyr::bind_rows(out, current)

    cu <- sf::st_union(sf::st_geometry(current)) # union of just-added
    u <- if (is.null(u)) cu else sf::st_union(u, cu)

    if (!quiet) {
      message(sprintf(
        "[%d/%d] kept (attr=%s)",
        i,
        n,
        as.character(current[[attr]][1])
      ))
    }
  }

  # final clean-up (optional: keep polygons/multipolygons only)
  out <- out[!sf::st_is_empty(out), , drop = FALSE]
  # out <- sf::st_collection_extract(out, "POLYGON")  # uncomment if needed

  out
}


#' Remove narrow / linear parts using negative buffer, but keep original borders
#'
#' @param x sf POLYGON/MULTIPOLYGON
#' @param dist positive buffer distance to "eat" narrow parts (e.g. 3 for 3 m)
#' @param by_feature logical, buffer each feature separately (safer) or once for all (faster)
#'
#' @return sf
drop_slivers_buffer <- function(x, dist = 3, by_feature = TRUE) {
  stopifnot(inherits(x, "sf"))
  stopifnot(dist > 0)

  # ensure polygons
  x <- st_cast(x, "MULTIPOLYGON", warn = FALSE)

  # add explicit id
  x <- x %>%
    mutate(.id_orig = dplyr::row_number())

  # keep attribute table separate (no geometry)
  attrs <- x %>% st_drop_geometry()

  if (by_feature) {
    # 1) erode each feature
    eroded <- st_buffer(x, dist = -dist)
    eroded <- st_make_valid(eroded)

    # 2) restore each feature
    restored <- st_buffer(eroded, dist = dist)
    restored <- st_make_valid(restored)

    # 3) intersect using geometry + id only
    orig_id <- x %>% select(.id_orig)
    rest_id <- restored %>% select(.id_orig)

    cleaned <- st_intersection(orig_id, rest_id)

    # 4) join attrs back once
    cleaned <- cleaned %>%
      left_join(attrs, by = ".id_orig") %>%
      select(-.id_orig)

    return(cleaned)
  } else {
    # by_feature = FALSE: one big mask
    x_u <- st_union(x)

    eroded <- st_buffer(x_u, dist = -dist) %>% st_make_valid()
    restored <- st_buffer(eroded, dist = dist) %>% st_make_valid()

    # turn restored into sf (one feature or few)
    restored_sf <- restored %>% st_as_sf()

    # intersect original (with ids) with mask (no attrs)
    cleaned <- st_intersection(
      x %>% select(.id_orig),
      restored_sf %>% select()
    )

    # join attrs back once
    cleaned <- cleaned %>%
      left_join(attrs, by = ".id_orig") %>%
      select(-.id_orig)

    return(cleaned)
  }
}
