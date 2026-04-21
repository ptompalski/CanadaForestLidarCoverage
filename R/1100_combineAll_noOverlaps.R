source("R/0000_setup.R")

#availability codes
# 0 - not available (most of AB)
# 1 - all data available, easy access through a web portal (e.g. BC, NS)
# 2 - all data available but access is complicated, no web portal etc (ON)
# 3 - only raster products are available (DTM, DSM etc) QC?

ALS_BC_diss <- st_read("layers/pre-processed/BC/ALS_BC_diss.gpkg")
ALS_QC_diss <- st_read("layers/pre-processed/QC/ALS_QC_diss.gpkg")
ALS_AB_diss <- st_read("layers/pre-processed/AB/ALS_AB_diss.gpkg")
ALS_NS_diss <- st_read("layers/pre-processed/NS/ALS_NS_diss.gpkg")
ALS_SK_diss <- st_read("layers/pre-processed/SK/ALS_SK_diss.gpkg")
ALS_NB_diss <- st_read("layers/pre-processed/NB/ALS_NB_diss.gpkg")
ALS_PEI_diss <- st_read("layers/pre-processed/PEI/ALS_PEI_diss.gpkg")
ALS_ON_diss <- st_read("layers/pre-processed/ON/ALS_ON_diss.gpkg")


# ### ON ####
# ALS_ON <- st_read("layers/source_layers/ON/ALS_ON_2018-2024_wDensity.gpkg")

# # remove overlap - temporary solution. There are currently no multi-temporal data in ON (except Hears, Romeo, PRF)
# out <- remove_overlaps_by_attr(ALS_ON, "Year")

# # # Sort polygons by year (oldest first, so newer ones overwrite)
# # ALS_ON <- ALS_ON %>% arrange(ALS_ON)

# # # Initialize output
# # out <- ALS_ON[0, ]

# # # Iteratively remove overlaps from older years
# # for (i in seq_len(nrow(ALS_ON))) {
# #   current <- ALS_ON[i, ]

# #   # remove parts of 'current' that overlap with already kept polygons
# #   if (nrow(out) > 0) {
# #     current <- st_difference(current, st_union(out))
# #   }

# #   # add the (possibly trimmed) polygon to the output
# #   out <- rbind(out, current)
# # }

# ALS_ON_diss <-
#   out %>%
#   select(YEAR = Year, PPM) %>%
#   mutate(area = st_area(.)) %>%
#   # group_by(YEAR, PPM) %>%
#   # summarise(n=n(), area=sum(area), geometry = st_union(geometry)) %>%
#   mutate(Province = "ON") %>%
#   mutate(isAvailable = 2) %>%
#   st_cast() %>%
#   # mutate(n = NA) %>%
#   select(Province, YEAR, PPM, area, isAvailable)

# # ALS_ON_diss <- ALS_ON_diss %>% rename(geometry = geom)
# st_write(ALS_ON_diss, "layers/pre-processed/ON/ALS_ON_diss.gpkg", append=F)

### combine into one layer ####

D <- rbind(
  ALS_AB_diss,
  ALS_BC_diss,
  ALS_NB_diss,
  ALS_ON_diss,
  ALS_QC_diss,
  ALS_PEI_diss,
  ALS_NS_diss,
  ALS_SK_diss
)

#make sure year is a number
D$YEAR <- as.numeric(D$YEAR)

## CLIP TO FORESTED ECOZONEZES ##
#new step added on 2024-09-2024
ecozones_forested <- st_read(forested_ecozones_path)
ecozones_forested %<>% st_transform(crs = the_crs)
D <- sf::st_intersection(D, ecozones_forested)

saveRDS(D, glue("layers/ALS_coverage_layer/main/ALS_coverage_all_{ver}.rds"))

# st_write(D, glue("ALS_coverage_all_{ver}.gpkg"), append=F)

#postprocess the acquisition polygons

# clip to provinces vector (no areas outside)
provinces <- st_read(canada_provinces_path)
provinces2 <- provinces %>%
  group_by(PROV) %>%
  summarise(n = n(), area = sum(Shape_Area)) %>%
  mutate(PROV = toupper(PROV)) %>%
  st_cast()


Dx <- st_intersection(D, provinces2)
saveRDS(Dx, "layers/ALS_coverage_all_2025_clipped.rds")

# dissolve all to create a single poly
D1 <- Dx %>% ungroup() %>% summarise(n = n()) %>% st_cast()

#remove holes
# library(nngeo)
D1 <- nngeo::st_remove_holes(D1, max_area = 2500000)

D1 <- rmapshaper::ms_simplify(D1, keep = 0.02)

# st_write(D1, glue("ALS_coverage_all_{ver}_generalized_v2.gpkg"), append=F)
st_write(
  D1,
  glue(
    "layers/ALS_coverage_layer/generalized/ALS_coverage_all_{ver}_generalized_v2.gpkg"
  ),
  append = F
)
