#most recent update July 17, 2025 (kmz file shared by Joanne)
#most recent update September 11, 2024 (email from Lane Gelhorn to Joanne)
#
#one shp, no need to run the preprocessing script

ALS_SK <- st_read("layers/source_layers/SK/SK_combined.gpkg")
st_geometry(ALS_SK) <- "geometry"

ALS_SK <- ALS_SK %>%
  st_make_valid() %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry)) %>%
  mutate(Province = "SK") %>%
  mutate(isAvailable = 1) %>%
  select(Province, YEAR, PPM, area, isAvailable) %>%
  st_as_sf()

st_write(ALS_SK, dsn = "layers/pre-processed/SK/ALS_SK.gpkg", append = F)

# without overlaps - newest acquisition kept
ALS_SK_diss <- remove_overlaps_by_attr(ALS_SK, "YEAR")

#update area
ALS_SK_diss <- ALS_SK_diss %>% mutate(area = st_area(geometry))

st_write(
  ALS_SK_diss,
  dsn = "layers/pre-processed/SK/ALS_SK_diss.gpkg",
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
