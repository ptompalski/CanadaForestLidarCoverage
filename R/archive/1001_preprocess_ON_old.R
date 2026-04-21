source("R/0000_setup.R")

# preprocess ON data

#load ON data with density
ALS_ON <- st_read("layers/source_layers/ON/ALS_ON_wDensity_2018-2023.shp")
ALS_ON %<>% select(Year, PPM = dnstyPls_m)

### 2024 data ####
ALS_ON_2024 <- st_read(
  "layers/source_layers/ON/y7_omnr_acquired_area_combined_ALL_zones_20240918.shp"
)
ALS_ON_2024 %<>% st_transform(crs = the_crs)
ALS_ON_2024 <- st_zm(ALS_ON_2024)
ALS_ON_2024 <- ALS_ON_2024 %>%
  mutate(Year = 2024) %>%
  group_by(Year) %>%
  summarize(geometry = st_union(geometry))
ALS_ON_2024 %<>% mutate(PPM = 50)

ALS_ON <- bind_rows(ALS_ON, ALS_ON_2024)

# round the density
ALS_ON <- ALS_ON %>% mutate(PPM = round(PPM))


#dissolve by year and PPM
ALS_ON <- ALS_ON %>%
  group_by(YEAR = Year, PPM) %>%
  summarize(geometry = st_union(geometry))

ALS_ON <- ALS_ON %>%
  st_make_valid() %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry)) %>%
  mutate(Province = "ON") %>%
  mutate(isAvailable = 2) %>%
  relocate(Province, YEAR, PPM, area, isAvailable) %>%
  st_as_sf()

st_write(ALS_ON, dsn = "layers/pre-processed/ON/ALS_ON.gpkg", append = F)

# without overlaps - newest acquisition kept
ALS_ON_diss <- remove_overlaps_by_attr(ALS_ON, "YEAR")

#update area
ALS_ON_diss <- ALS_ON_diss %>% mutate(area = st_area(geometry))

st_write(
  ALS_ON_diss,
  dsn = "layers/pre-processed/ON/ALS_ON_diss.gpkg",
  append = F
)

# st_write(ALS_ON, dsn = "layers/ON/ALS_ON_2018-2024_wDensity.gpkg")

# ----
# Important
# the script below is the original script to process ON data
# However, because the data didn't have PPM, after processing, additional script was run
# to download random tiles in each acquisition and obtain point density
# This script now lives in 9999_getDensity_ON.R
# It produces the file ALS_ON_2018-2024_wDensity.gpkg
# ----

# # most recent update - 2024-09-23

# ### 2022 data ####
# flist <- list.files(
#   "layers/source_layers/ON/2022_Deliverable_Shapefiles_Final/",
#   pattern = "shp$",
#   full.names = T
# )

# X <- map(flist, st_read)
# X <- map(X, st_union)
# X <- map(X, st_transform, crs = the_crs)
# X <- st_union(do.call("c", X))
# X <- st_zm(X)

# st_write(X, dsn = "layers/pre-processed/ON/2022_combined.shp", append=F)

# ### 2023 data ####

# flist <- list.files(
#   "layers/source_layers/ON/2023_Delivered_Tile_Index/",
#   pattern = "shp$",
#   full.names = T
# )

# X <- map(flist, st_read)
# X <- map(X, st_union)
# X <- map(X, st_transform, crs = the_crs)
# X <- st_union(do.call("c", X))
# X <- st_zm(X)

# st_write(X, dsn = "layers/pre-processed/ON/2023_combined.shp", append=F)

# ### 2024 data ####
# ALS_ON_2024 <- st_read(
#   "layers/source_layers/ON/y7_omnr_acquired_area_combined_ALL_zones_20240918.shp"
# )
# ALS_ON_2024 %<>% st_transform(crs = the_crs)
# ALS_ON_2024 <- st_zm(ALS_ON_2024)
# ALS_ON_2024 <- ALS_ON_2024 %>%
#   mutate(Year = 2024) %>%
#   group_by(Year) %>%
#   summarize(geometry = st_union(geometry))

# #####

# ALS_ON <- st_read("layers/source_layers/ON/OMNR_Y1_to_Y4_Blocks.shp")
# ALS_ON_2022 <- st_read("layers/pre-processed/ON/2022_combined.shp")
# ALS_ON_2023 <- st_read("layers/pre-processed/ON/2023_combined.shp")

# ALS_ON <- ALS_ON %>%
#   st_zm() %>%
#   st_transform(crs = the_crs) %>%
#   group_by(Year) %>%
#   summarize(geometry = st_union(geometry))

# #make sure names in both layers match
# ALS_ON_2022 <- ALS_ON_2022 %>% mutate(Year = 2022) %>% select(Year, geometry)
# ALS_ON_2023 <- ALS_ON_2023 %>% mutate(Year = 2023) %>% select(Year, geometry)

# ALS_ON <- rbind(ALS_ON, ALS_ON_2022)
# ALS_ON <- rbind(ALS_ON, ALS_ON_2023)
# ALS_ON <- rbind(ALS_ON, ALS_ON_2024)

# # st_write(ALS_ON, dsn = "layers/ON/ALS_ON_2018-2023.shp")
# st_write(ALS_ON, dsn = "layers/pre-processed/ON/ALS_ON_2018-2024.gpkg")

#there is no URLS to download the 2024 data and the density is not provided.
# Using the average density for data acquired in 2023, which is ~ 50 pts/m2

# x <- st_read("layers/ON/ALS_ON_wDensity_2018-2023.shp")
# x %>% group_by(Year) %>% summarise(mean(dnstyPts_m))
