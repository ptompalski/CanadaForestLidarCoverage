# purpose of this script is to calculate summary statistics needed for the Table on the first page:
# Jurisdiction, Total ALS coverage (area + %), ALS coverage over managed forest (area + %)

# dissolved coverage layer is used (i.e. if there is overlap, newest acquisition is used) so that
# the area is not counted twice.

source("R/0000_setup.R")

coverage_managed_unmanaged_file <- Sys.getenv(
  "COVERAGE_MANAGED_UNMANAGED_FILE",
  unset = "layers/coverageManagedUnmanaged.rds"
)
stats_file <- Sys.getenv("STATS_FILE", unset = "layers/Stats.rds")
data_for_figure_file <- Sys.getenv(
  "DATA_FOR_FIGURE_FILE",
  unset = "layers/dataForTheFigure.rds"
)

# D is loaded already in maps_setup

# Dissolve
D1 <- D %>% group_by(Province) %>% summarize()
ALS_area_dissolved_by_province <-
  D1 %>%
  mutate(total_area_dissolved = set_units(st_area(.), km^2)) %>%
  select(
    jurisdiction_code = Province,
    total_ALS_area = total_area_dissolved
  ) %>%
  as_tibble() %>%
  select(-geom)

#check if dissolve worked correctly - update: it did
# D1_dissolved <- D1 %>%
#   mutate(total_area_dissolved = set_units(st_area(.), km^2)) %>%
#   select(jurisdiction_code=Province, total_ALS_area = total_area_dissolved)
# st_write(D1_dissolved, "../5_lidarStatusCanada/ALS_coverage_all_2024_dissolveTest.gpkg")
# D1_dissolved_poly <- D1_dissolved %>%
#   st_cast("POLYGON")
# D1_dissolved_poly %<>% mutate(total_area_dissolved = set_units(st_area(.), km^2))
# sum(D1_dissolved_poly$total_area_dissolved) #1725747

total_ALS_area <- sum(ALS_area_dissolved_by_province$total_ALS_area)
#in m2 = 1725747168596

# table

#values copied from Table 2 in the manuscript
theTable <- provinces_area %>%
  mutate(
    total_forest_area_perc = round(total_forest_area / total_area * 100, 1),
    managed_forest_area_perc = round(managed_forest_area / total_area * 100, 1)
  )

# add
# total ALS area (dissolved!) by jurisdiction

theTable %<>%
  left_join(ALS_area_dissolved_by_province) %>%
  mutate(
    total_ALS_area_perc = as.numeric(total_ALS_area / total_area * 100),
    #cap at 100
    total_ALS_area_perc = if_else(
      total_ALS_area_perc > 100,
      100,
      total_ALS_area_perc
    )
  )


# total ALS area (dissolved!) for managed forests, by jurisdiction
# note - these areas are in ha

# coverageManagedUnmanaged <- readRDS("../5_lidarStatusCanada/coverageManagedUnmanaged.rds")
coverageManagedUnmanaged <- readRDS(coverage_managed_unmanaged_file)
# sum(coverageManagedUnmanaged$area_ALS, na.rm = T) #small difference from the area calculated using the shapefile, but very similar.
# 172575044 ha
# (172575044 / 100 ) -#area in km2
# as.numeric(total_ALS_area)
#difference is only 3.27 km2

#total area of each jurisdiction?
# coverageManagedUnmanaged %>%
#   group_by(jurisdiction) %>%
#   summarise(sum(area_prov))

#proportion of ALS coverage for managed forests
ALS_area_managed <-
  coverageManagedUnmanaged %>%
  filter(forest_type == "managed") %>%
  #convert from ha to km2
  mutate(across(starts_with("area"), ~ .x / 100)) %>%
  select(
    jurisdiction_code = jurisdiction,
    managed_ALS_area = area_ALS,
    managed_ALS_perc = prop
  )

# ALS_area_managed <- coverageManagedUnmanaged %>%
#   filter(forest_type == "managed") %>%
#   mutate(across(starts_with("area"), ~.x / 100)) %>%
#   select(jurisdiction_code=jurisdiction, managed_ALS_area=area_ALS)

theTable %<>% left_join(ALS_area_managed)


#stats for entire Canada
total_ALS_area <- round(as.numeric(sum(theTable$total_ALS_area, na.rm = T)))


CanadaLandArea <- 9093507
# CanadaManagedArea <- 250*250 *  30055435 / 1000000 #30055435 32824330
#<- round(as.numeric(sum(theTable$managed_forest_area, na.rm=T)))

total_ALS_perc <- round(total_ALS_area / CanadaLandArea * 100, 1)

# managed ALS perc
# managed_ALS_perc  <- round(total_ALS_area/ CanadaManagedArea * 100,1) #this is wrong
managed_ALS_perc <-
  coverageManagedUnmanaged %>%
  filter(forest_type == "managed") %>%
  select(-prop) %>%
  summarise(
    area_prov = sum(area_prov), #this is the total managed area of the province
    area_ALS = sum(area_ALS, na.rm = T)
  ) %>%
  mutate(prop = area_ALS / area_prov * 100) %>%
  pull(prop)

theTableCopy <- theTable

theTable <-
  theTable %>%
  filter(!is.na(total_ALS_area)) %>%

  #nicer jurisdiction names (with codes in brackets)
  mutate(
    Jurisdiction = paste0(jurisdiction_name, " (", jurisdiction_code, ")")
  ) %>%
  relocate(Jurisdiction) %>%
  select(
    -jurisdiction_name,
    -jurisdiction_code,
    -total_area,
    -total_forest_area,
    -managed_forest_area,
    -total_forest_area_perc,
    -managed_forest_area_perc
  ) %>%

  mutate(
    `Total ALS coverage` = paste0(
      format(
        round(as.numeric(total_ALS_area)),
        big.mark = ",",
        scientific = FALSE
      ),
      " (",
      round(total_ALS_area_perc, 1),
      "%)"
    )
  ) %>%
  select(-total_ALS_area_perc, -total_ALS_area) %>%

  mutate(
    `ALS coverage over managed forest` = paste0(
      format(
        round(as.numeric(managed_ALS_area)),
        big.mark = ",",
        scientific = FALSE
      ),
      " (",
      round(managed_ALS_perc, 1),
      "%)"
    )
  ) %>%
  select(-managed_ALS_perc, -managed_ALS_area)


# coverage of all forested ecozones?
D_dissolved <- D %>% summarize(geometry = st_union(geom))

ecozones_forested <- st_read(forested_ecozones_path)
ecozones_forested %<>% st_transform(crs = the_crs)


coverage_forested <- st_intersection(D_dissolved, ecozones_forested)
# coverage_forested <- st_union(D_dissolved, ecozones_forested)

forested_ALS_perc <- as.numeric(
  st_area(coverage_forested) / st_area(ecozones_forested) * 100
)


#objects to save
Stats <- list(
  theTable = theTable,
  total_ALS_area = total_ALS_area,
  total_ALS_perc = total_ALS_perc,
  forested_ALS_perc = forested_ALS_perc,
  managed_ALS_perc = managed_ALS_perc
)

saveRDS(Stats, stats_file)


#data for the figure (this is copied from 3000_stats_revised.R)
DD <- D %>% as_tibble() %>% dplyr::select(-geom)

#cumulative ALS area by province
DF_area <- DD %>%
  mutate(YEAR2 = as.numeric(YEAR)) %>%
  filter(Province != "MB") %>%
  group_by(Province, YEAR2) %>%
  summarise(Area_km2 = as.numeric(sum(area)) * 1e-6) %>%
  left_join(provinces_area, by = join_by("Province" == "jurisdiction_code")) %>%
  mutate(
    Area_km2_cumulative = cumsum(Area_km2)
    #Area_km2_cumulative_perc = Area_km2_cumulative / total_area * 100
  )

DF_area %<>% rename(Jurisdiction = Province)

#PEI only to make it on top of SK
DF_area_PE <- DF_area %>% filter(Jurisdiction == "PE")
#same for NB
DF_area_NB <- DF_area %>% filter(Jurisdiction == "NB")


DF_area_recent <- DF_area %>%
  group_by(Jurisdiction) %>%
  slice_max(YEAR2) %>%
  select(Jurisdiction, YEAR2, Area_km2_cumulative)

DF_area_recent <- theTableCopy %>%
  select(
    Jurisdiction = jurisdiction_code,
    perc_cover_managed = managed_ALS_perc,
    perc_cover_prov = total_ALS_area_perc
  ) %>%
  left_join(DF_area_recent)


#create labels
DF_area_recent <-
  DF_area_recent %>%
  dplyr::select(
    Jurisdiction,
    YEAR2,
    perc_cover_managed,
    perc_cover_prov,
    Area_km2_cumulative
  ) %>%
  mutate(
    managed_perc_label = paste0(round(perc_cover_managed, 1), "%"),
    area_perc_label = paste0(round(perc_cover_prov, 1), "%"),

    label_all = glue(
      "<b>{Jurisdiction}</b><br>P<sub>J</sub>={area_perc_label}<br>P<sub>M</sub>={managed_perc_label}"
    )
  )


label_position_adj <-
  tribble(
    ~Jurisdiction , ~adj_x , ~adj_y ,
    "AB"          , -3     ,      0 ,
    "BC"          ,  0.25  ,    -25 ,
    "QC"          ,  0.25  ,     20 ,
    "ON"          ,  0.25  ,     15 ,
    "NS"          , -0.5   ,     50 ,
    "NB"          , -2.5   ,     60 ,
    "SK"          ,  0.25  ,      0 ,
    "PE"          ,  2.25  ,     10
  )

DF_area_recent_adj <- DF_area_recent %>%
  left_join(label_position_adj) %>%
  mutate(x = YEAR2 + adj_x, y = Area_km2_cumulative / 1000 + adj_y)


dataForTheFigure <- list(
  DF_area = DF_area,
  DF_area_PE = DF_area_PE,
  DF_area_NB = DF_area_NB,
  DF_area_recent_adj = DF_area_recent_adj
)

saveRDS(dataForTheFigure, data_for_figure_file)
