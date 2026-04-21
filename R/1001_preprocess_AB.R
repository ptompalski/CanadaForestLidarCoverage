# Preprocessing acquisition data for AB

#update 2024-06-24 - added ABMI coverage

ALS_AB_for1 <- st_read(
  "layers/source_layers/AB/shapfiles/forestry_division/clean_laz_tiles_z11.shp"
)
ALS_AB_for2 <- st_read(
  "layers/source_layers/AB/shapfiles/forestry_division/clean_laz_tiles_z12.shp"
)
ALS_AB_riv1 <- st_read(
  "layers/source_layers/AB/shapfiles/river_engineering/river_engineering_laz_tiles_z11.shp"
)
ALS_AB_riv2 <- st_read(
  "layers/source_layers/AB/shapfiles/river_engineering/river_engineering_laz_tiles_z12.shp"
)
ALS_AB_gp <- st_read(
  "layers/source_layers/AB/shapfiles/county_of_grande_prarie/grande_prairie_clean_z11.shp"
)
# ALS_AB_ABMI <- st_read("layers/AB/shapfiles/LiDAR_Imagery_ABMI_2022_24.shp")  #received 2024-06-26
# ALS_AB_ABMI <- st_read("layers/source_layers/AB/shapfiles/ABMI/LiDAR_Imagery_ABMI_2022_2024_26July24.shp")  #received 2024-07-26
ALS_AB_ABMI <- st_read(
  "layers/source_layers/AB/shapfiles/ABMI/LiDAR_Imagery_ABMI_2022_2025_Oct2025.shp"
)


ALS_AB_ABMI %<>%
  # filter(YEAR != "Plan 2024") %>%
  mutate(YEAR = as.numeric(LIDAR_yr))

# if ppm is 0 then data processing not finished - assign 12
ALS_AB_ABMI %<>% mutate(AveragePPM = if_else(AveragePPM == 0, 12, AveragePPM))


#remove the ALS acquisition outside AB (located in BC)
# ALS_AB_ABMI %<>% filter(NewNameAOI != "Fort St John Perimeter Forest")

ALS_AB_for1 %<>% st_transform(crs = the_crs) %>% select(year, pnt_den)
ALS_AB_for2 %<>% st_transform(crs = the_crs) %>% select(year, pnt_den)
ALS_AB_riv1 %<>% st_transform(crs = the_crs) %>% select(year, pnt_den)
ALS_AB_riv2 %<>% st_transform(crs = the_crs) %>% select(year, pnt_den)
ALS_AB_gp %<>% st_transform(crs = the_crs) %>% select(year, pnt_den)
ALS_AB_ABMI %<>% st_transform(crs = the_crs)

ALS_AB_for1 %<>% mutate(isAvailable = 0)
ALS_AB_for2 %<>% mutate(isAvailable = 0)
ALS_AB_riv1 %<>% mutate(isAvailable = 1)
ALS_AB_riv2 %<>% mutate(isAvailable = 1)
ALS_AB_gp %<>% mutate(isAvailable = 1)
ALS_AB_ABMI %<>% mutate(isAvailable = 1)


ALS_AB <- rbind(ALS_AB_for1, ALS_AB_for2, ALS_AB_riv1, ALS_AB_riv2, ALS_AB_gp)

#dissolve by year and availability, calculate average PPM
ALS_AB <-
  ALS_AB %>%
  select(YEAR = year, PPM = pnt_den, isAvailable) %>%
  st_make_valid() %>%
  mutate(area = st_area(.)) %>%
  group_by(YEAR, isAvailable) %>%
  summarise(
    PPM = mean(PPM),
    # n = n(),
    area = sum(area),
    geometry = st_union(geometry)
  ) %>%
  mutate(Province = "AB") %>%
  st_cast() %>%
  select(Province, YEAR, PPM, area, isAvailable)


#remove holes
library(nngeo)
ALS_AB <- nngeo::st_remove_holes(ALS_AB)


# st_write(ALS_AB_diss_noHoles, "layers/AB/AB_shapefiles_combined_noHoles.gpkg")

ALS_AB_ABMI <-
  ALS_AB_ABMI %>%
  # mutate(PPM=12) %>%
  select(YEAR, PPM = AveragePPM, isAvailable) %>%
  st_make_valid() %>%
  mutate(area = st_area(.)) %>%
  group_by(YEAR, PPM, isAvailable) %>%
  summarise(
    PPM = mean(PPM),
    # n = n(),
    area = sum(area),
    geometry = st_union(geometry)
  ) %>%
  mutate(Province = "AB") %>%
  st_cast() %>%
  select(Province, YEAR, PPM, area, isAvailable)

#combine all AB acquisitions
ALS_AB <-
  bind_rows(ALS_AB, ALS_AB_ABMI)

#save
st_write(ALS_AB, dsn = "layers/pre-processed/AB/ALS_AB.gpkg", append = F)

# without overlaps - newest acquisition kept
ALS_AB_diss <- remove_overlaps_by_attr(ALS_AB, "YEAR")

#update area
ALS_AB_diss <- ALS_AB_diss %>% mutate(area = st_area(geometry))

st_write(
  ALS_AB_diss,
  dsn = "layers/pre-processed/AB/ALS_AB_diss.gpkg",
  append = F
)
