#preprocess QC acquision

source("R/0000_setup.R")

# file downloaded from https://diffusion.mern.gouv.qc.ca/diffusion/RGQ/Imagerie/Index/Lidar/
# P <- st_layers("layers/source_layers/QC/INDEX_LiDAR.gpkg")
qc_source_file <- "layers/source_layers/QC/INDEX_LiDAR.gpkg"
qc_source_layer <- "Index_Tuiles_LiDAR"
qc_output_dir <- Sys.getenv("QC_OUTPUT_DIR", unset = "layers/pre-processed/QC")
qc_output_file <- Sys.getenv(
  "QC_OUTPUT_FILE",
  unset = file.path(qc_output_dir, "ALS_QC.gpkg")
)
qc_output_diss_file <- Sys.getenv(
  "QC_OUTPUT_DISS_FILE",
  unset = file.path(qc_output_dir, "ALS_QC_diss.gpkg")
)

dir_create(dirname(qc_output_file))
dir_create(dirname(qc_output_diss_file))

ALS_QC <- read_sf(
  qc_source_file,
  layer = qc_source_layer
)

#standardise geometry column (sometimes is called "geom")
st_geometry(ALS_QC) <- "geometry"

# transform to common CRS
ALS_QC <- ALS_QC %>% st_transform(crs = the_crs)

# keep relevant columns
ALS_QC <- ALS_QC %>%
  select(YEAR = ANNEE_FIN, PPM = DENSITE_POINT_M2)

# Drop empties + fix invalid features
ALS_QC <- ALS_QC %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid()

# drop rows with density of 999
ALS_QC <- ALS_QC %>% filter(PPM != 999)

# Safe group-wise union
ALS_QC <- ALS_QC %>%
  group_by(YEAR, PPM) %>%
  summarise(
    geometry = st_union((geometry)),
    .groups = "drop"
  ) %>%
  st_make_valid()


ALS_QC <- ALS_QC %>%
  # st_make_valid() %>%
  # st_as_sf() %>%
  mutate(area = st_area(geometry)) %>%
  mutate(Province = "QC") %>%
  mutate(isAvailable = 1) %>%
  relocate(Province, YEAR, PPM, area, isAvailable)

#save
st_write(
  ALS_QC,
  dsn = qc_output_file,
  append = F
)


#version without overlaps
ALS_QC_diss <- remove_overlaps_by_attr(ALS_QC, "YEAR")

st_write(
  ALS_QC_diss,
  dsn = qc_output_diss_file,
  append = F
)

# P$YEAR <- P$ANNEE_FIN

# P <- P %>%
#   st_as_sf() %>%
#   filter(!is.na(ANNEE_FIN)) %>%
#   filter(!st_is_empty(geom))

# P <- P %>% mutate(.id = row_number())

# # ---------------------- BUILD CANDIDATE PAIRS --------------------------------
# # Use sparse index of true overlaps (excludes boundary-only touches)
# ov_idx <- st_overlaps(P, sparse = TRUE)

# # Convert to a tidy pairs table (id_a < id_b to avoid duplicates)
# pairs_tbl <- tibble(
#   id_a = seq_along(ov_idx),
#   id_b_list = ov_idx
# ) %>%
#   unnest_longer(id_b_list, values_to = "id_b") %>%
#   filter(id_a < id_b)

# overlaps <- pmap_dfr(
#   list(pairs_tbl$id_a, pairs_tbl$id_b),
#   function(a, b) {
#     A <- P[P$.id == a, ]
#     B <- P[P$.id == b, ]
#     I <- suppressWarnings(st_intersection(A, B))
#     if (nrow(I) == 0 || any(st_is_empty(I))) {
#       return(NULL)
#     }

#     # Keep attributes from both polygons
#     I %>%
#       mutate(
#         id_a = a,
#         id_b = b,
#         YEAR_a = A$YEAR[[1]],
#         YEAR_b = B$YEAR[[1]]
#       )
#   }
# )

# # Drop empty just in case, compute areas & tidy attributes

# overlaps <- overlaps %>%
#   filter(!st_is_empty(geom)) %>%
#   mutate(
#     overlap_area_m2 = set_units(st_area(geom), m^2),
#     overlap_area_km2 = set_units(overlap_area_m2, km^2),
#     older_year = pmin(YEAR_a, YEAR_b, na.rm = TRUE),
#     newer_year = pmax(YEAR_a, YEAR_b, na.rm = TRUE),
#     delta_year = newer_year - older_year
#   ) %>%
#   # Reorder & keep just what’s useful; keep geometry
#   select(
#     id_a,
#     id_b,
#     YEAR_a,
#     YEAR_b,
#     older_year,
#     newer_year,
#     delta_year,
#     overlap_area_m2,
#     overlap_area_km2,
#     geom
#   ) %>%
#   # Optional: dissolve multipart intersections that may arise
#   st_make_valid()

# st_write(overlaps, "layers/pre-processed/QC/QC_overlaps.gpkg")

# plot(overlaps["delta_year"])
