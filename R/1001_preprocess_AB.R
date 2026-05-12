# Preprocessing acquisition data for AB
if (!exists("the_crs") || !exists("coverage_output_paths")) {
  source("R/0000_setup.R")
}

#update 2024-06-24 - added ABMI coverage

ab_source_dir <- "layers/source_layers/AB/shapfiles"
ab_output_paths <- coverage_output_paths("AB")
ab_province_boundaries <- read_province_boundaries() %>%
  st_transform(crs = the_crs) %>%
  select(Province = PROV)

ALS_AB_for1 <- st_read(
  file.path(ab_source_dir, "forestry_division/clean_laz_tiles_z11.shp")
)
ALS_AB_for2 <- st_read(
  file.path(ab_source_dir, "forestry_division/clean_laz_tiles_z12.shp")
)
ALS_AB_riv1 <- st_read(
  file.path(ab_source_dir, "river_engineering/river_engineering_laz_tiles_z11.shp")
)
ALS_AB_riv2 <- st_read(
  file.path(ab_source_dir, "river_engineering/river_engineering_laz_tiles_z12.shp")
)
ALS_AB_gp <- st_read(
  file.path(ab_source_dir, "county_of_grande_prarie/grande_prairie_clean_z11.shp")
)
# ALS_AB_ABMI <- st_read("layers/AB/shapfiles/LiDAR_Imagery_ABMI_2022_24.shp")  #received 2024-06-26
# ALS_AB_ABMI <- st_read("layers/source_layers/AB/shapfiles/ABMI/LiDAR_Imagery_ABMI_2022_2024_26July24.shp")  #received 2024-07-26
ALS_AB_ABMI <- st_read(
  file.path(ab_source_dir, "ABMI/LiDAR_Imagery_ABMI_2022_2025_Oct2025.shp")
)
ALS_AB_FRIP <- st_read(
  file.path(ab_source_dir, "FRIP/LiDAR_AOI_Alberta_info_diss.shp")
)


ALS_AB_ABMI %<>%
  # filter(YEAR != "Plan 2024") %>%
  mutate(YEAR = as.numeric(LIDAR_yr))
ALS_AB_FRIP %<>%
  mutate(
    YEAR = as.numeric(LiDAR_yr),
    PPM = as.numeric(LiDAR_ppm)
  )

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
ALS_AB_FRIP %<>% st_transform(crs = the_crs)

ALS_AB_for1 %<>% mutate(isAvailable = 0)
ALS_AB_for2 %<>% mutate(isAvailable = 0)
ALS_AB_riv1 %<>% mutate(isAvailable = 1)
ALS_AB_riv2 %<>% mutate(isAvailable = 1)
ALS_AB_gp %<>% mutate(isAvailable = 1)
ALS_AB_ABMI %<>% mutate(isAvailable = 1)
ALS_AB_FRIP %<>% mutate(isAvailable = 0)

ALS_AB_for1 %<>% add_source_metadata("AB Government of Alberta", "Restricted / not open")
ALS_AB_for2 %<>% add_source_metadata("AB Government of Alberta", "Restricted / not open")
ALS_AB_riv1 %<>% add_source_metadata("AB Government of Alberta", "Restricted / not open")
ALS_AB_riv2 %<>% add_source_metadata("AB Government of Alberta", "Restricted / not open")
ALS_AB_gp %<>% add_source_metadata("AB Government of Alberta", "Restricted / not open")
ALS_AB_ABMI %<>% add_source_metadata("AB ABMI", "Open point cloud and derivatives")
ALS_AB_FRIP %<>% add_source_metadata("AB Government of Alberta", "Restricted / not open")


ALS_AB <- rbind(ALS_AB_for1, ALS_AB_for2, ALS_AB_riv1, ALS_AB_riv2, ALS_AB_gp)

# dissolve by year and availability, calculate average PPM, then assign
# jurisdiction by location so cross-boundary acquisitions count where they lie.
ALS_AB <-
  ALS_AB %>%
  select(YEAR = year, PPM = pnt_den, isAvailable, source_provider, source_access) %>%
  st_make_valid() %>%
  mutate(area = st_area(.)) %>%
  group_by(YEAR, isAvailable, source_provider, source_access) %>%
  summarise(
    PPM = mean(PPM),
    # n = n(),
    area = sum(area),
    geometry = st_union(geometry)
  ) %>%
  assign_province_by_location(province_boundaries = ab_province_boundaries) %>%
  group_by(Province, YEAR, isAvailable, source_provider, source_access) %>%
  summarise(
    PPM = mean(PPM),
    area = sum(st_area(geometry)),
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  select(Province, YEAR, PPM, area, isAvailable, source_provider, source_access)


#remove holes
library(nngeo)
ALS_AB <- nngeo::st_remove_holes(ALS_AB)


# st_write(ALS_AB_diss_noHoles, "layers/AB/AB_shapefiles_combined_noHoles.gpkg")

ALS_AB_ABMI <-
  ALS_AB_ABMI %>%
  # mutate(PPM=12) %>%
  select(YEAR, PPM = AveragePPM, isAvailable, source_provider, source_access) %>%
  st_make_valid() %>%
  mutate(area = st_area(.)) %>%
  group_by(YEAR, PPM, isAvailable, source_provider, source_access) %>%
  summarise(
    PPM = mean(PPM),
    # n = n(),
    area = sum(area),
    geometry = st_union(geometry)
  ) %>%
  assign_province_by_location(province_boundaries = ab_province_boundaries) %>%
  group_by(Province, YEAR, PPM, isAvailable, source_provider, source_access) %>%
  summarise(
    area = sum(st_area(geometry)),
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  select(Province, YEAR, PPM, area, isAvailable, source_provider, source_access)

ALS_AB_FRIP <-
  ALS_AB_FRIP %>%
  select(YEAR, PPM, isAvailable, source_provider, source_access) %>%
  st_make_valid() %>%
  mutate(area = st_area(.)) %>%
  group_by(YEAR, PPM, isAvailable, source_provider, source_access) %>%
  summarise(
    area = sum(area),
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  assign_province_by_location(province_boundaries = ab_province_boundaries) %>%
  group_by(Province, YEAR, PPM, isAvailable, source_provider, source_access) %>%
  summarise(
    area = sum(st_area(geometry)),
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  select(Province, YEAR, PPM, area, isAvailable, source_provider, source_access)

#combine all AB acquisitions
ALS_AB <-
  bind_rows(ALS_AB, ALS_AB_ABMI, ALS_AB_FRIP)

#save
st_write(ALS_AB, dsn = ab_output_paths$file, append = F)

# without overlaps - newest acquisition kept; for same-year overlaps, open
# sources are prioritized over restricted ones and FRIP fills remaining gaps.
ALS_AB_diss <- ALS_AB %>%
  mutate(
    open_access_priority = if_else(source_access == "Open point cloud and derivatives", 1, 0),
    overlap_priority = YEAR * 100 + open_access_priority * 10 + isAvailable
  ) %>%
  remove_overlaps_by_attr("overlap_priority") %>%
  select(-open_access_priority, -overlap_priority)

#update area
ALS_AB_diss <- ALS_AB_diss %>% mutate(area = units::set_units(as.numeric(st_area(geometry)), m^2))

st_write(
  ALS_AB_diss,
  dsn = ab_output_paths$diss_file,
  append = F
)
