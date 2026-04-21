#most recent update July 17, 2025 (kmz file shared by Joanne)
#most recent update September 11, 2024 (email from Lane Gelhorn to Joanne)
#
#one shp, no need to run the preprocessing script

sk_output_dir <- Sys.getenv("SK_OUTPUT_DIR", unset = "layers/pre-processed/SK")
sk_output_file <- Sys.getenv(
  "SK_OUTPUT_FILE",
  unset = file.path(sk_output_dir, "ALS_SK.gpkg")
)
sk_output_diss_file <- Sys.getenv(
  "SK_OUTPUT_DISS_FILE",
  unset = file.path(sk_output_dir, "ALS_SK_diss.gpkg")
)

dir_create(dirname(sk_output_file))
dir_create(dirname(sk_output_diss_file))

ALS_SK <- st_read("layers/source_layers/SK/SK_combined.gpkg")
st_geometry(ALS_SK) <- "geometry"

ALS_SK <- ALS_SK %>% finalize_available_coverage("SK")

st_write(ALS_SK, dsn = sk_output_file, append = F)

# without overlaps - newest acquisition kept
ALS_SK_diss <- remove_overlaps_by_attr(ALS_SK, "YEAR")

#update area
ALS_SK_diss <- ALS_SK_diss %>% mutate(area = st_area(geometry))

st_write(
  ALS_SK_diss,
  dsn = sk_output_diss_file,
  append = F
)

# ALS_SK_diss <-
#   ALS_SK %>%
#   st_transform(crs = the_crs) %>%
#   mutate(
#     Province="SK",
#     # YEAR = 2022,
#     # PPM = NA,
#     # isAvailable=2,
#     isAvailable=case_when(
#       DATA_OWNER=="Public" ~ 1,
#       DATA_OWNER=="Partnership" ~ 3,
#       DATA_OWNER=="Private" ~ 0,
#     ),
#
#     n=1 ) %>%
#   mutate(area = st_area(.)) %>%
#   select(Province, YEAR, PPM=ANPD, n, area, isAvailable, geometry)
