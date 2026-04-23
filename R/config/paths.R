# current date - to append to the file names (data versioning)
ver <- Sys.getenv(
  "COVERAGE_VERSION",
  unset = today() %>% str_remove_all(pattern = "-")
)

the_crs <- 3978

logo_path <- "img/NRCan-CFS_logo.png"

forested_ecozones_path <- "layers/Forested_ecozones_cliped_dissolved.shp"
canada_provinces_path <- "layers/Canada_provinces.shp"
managed_forest_mask_path <- "layers/managed_forest_mask.tif"
