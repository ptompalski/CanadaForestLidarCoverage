library(sf)
library(dplyr)
library(units)

existing_file <- "layers/pre-processed/ON/ALS_ON.gpkg"
existing_diss_file <- "layers/pre-processed/ON/ALS_ON_diss.gpkg"

test_dir <- "scratch/on_refactor_test"
test_file <- file.path(test_dir, "ALS_ON_refactor_test.gpkg")
test_diss_file <- file.path(test_dir, "ALS_ON_diss_refactor_test.gpkg")

dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
unlink(c(test_file, test_diss_file))

Sys.setenv(
  ON_OUTPUT_FILE = test_file,
  ON_OUTPUT_DISS_FILE = test_diss_file
)

source("R/1001_preprocess_ON.R")

compare_layer <- function(existing_path, test_path, label) {
  existing <- st_read(existing_path, quiet = TRUE)
  test <- st_read(test_path, quiet = TRUE)

  existing_summary <- existing %>%
    st_drop_geometry() %>%
    group_by(Province, YEAR, PPM, isAvailable) %>%
    summarise(
      n = n(),
      area_sum = sum(as.numeric(area)),
      .groups = "drop"
    ) %>%
    arrange(Province, YEAR, PPM, isAvailable)

  test_summary <- test %>%
    st_drop_geometry() %>%
    group_by(Province, YEAR, PPM, isAvailable) %>%
    summarise(
      n = n(),
      area_sum = sum(as.numeric(area)),
      .groups = "drop"
    ) %>%
    arrange(Province, YEAR, PPM, isAvailable)

  existing_union <- existing %>%
    st_make_valid() %>%
    st_union()

  test_union <- test %>%
    st_make_valid() %>%
    st_union()

  diff_area_m2 <- st_sym_difference(existing_union, test_union) %>%
    st_area() %>%
    sum() %>%
    set_units("m^2") %>%
    as.numeric()

  list(
    label = label,
    existing_n = nrow(existing),
    test_n = nrow(test),
    same_names = identical(names(existing), names(test)),
    same_crs = identical(st_crs(existing), st_crs(test)),
    same_geometry_type = identical(
      unique(st_geometry_type(existing)),
      unique(st_geometry_type(test))
    ),
    same_summary = isTRUE(all.equal(existing_summary, test_summary, tolerance = 1e-6)),
    summary_difference = all.equal(existing_summary, test_summary, tolerance = 1e-6),
    symmetric_difference_m2 = diff_area_m2
  )
}

results <- list(
  compare_layer(existing_file, test_file, "ALS_ON"),
  compare_layer(existing_diss_file, test_diss_file, "ALS_ON_diss")
)

for (result in results) {
  cat("\n", result$label, "\n", sep = "")
  cat("  feature count: ", result$existing_n, " existing, ", result$test_n, " test\n", sep = "")
  cat("  same names: ", result$same_names, "\n", sep = "")
  cat("  same CRS: ", result$same_crs, "\n", sep = "")
  cat("  same geometry type: ", result$same_geometry_type, "\n", sep = "")
  cat("  same summary: ", result$same_summary, "\n", sep = "")
  cat(
    "  symmetric difference area: ",
    round(result$symmetric_difference_m2, 6),
    " m2\n",
    sep = ""
  )

  if (!isTRUE(result$same_summary)) {
    cat("  summary difference:\n")
    print(result$summary_difference)
  }
}

all_passed <- all(vapply(
  results,
  function(x) {
    x$same_names &&
      x$same_crs &&
      x$same_geometry_type &&
      x$same_summary &&
      isTRUE(all.equal(x$symmetric_difference_m2, 0, tolerance = 1e-6))
  },
  logical(1)
))

if (!all_passed) {
  stop("ON refactor output comparison failed", call. = FALSE)
}

cat("\nON refactor output comparison passed.\n")
