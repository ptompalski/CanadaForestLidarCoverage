#preprocess BC

source("R/0000_setup.R")

#newest updates in the bottom

dir_create("layers/pre-processed/BC")

bc_source_dir <- "layers/source_layers/BC"
bc_status_pattern <- file.path(bc_source_dir, "Provincial_ALS_Status_*.shp")
bc_status_available <- c(
  "Completed",
  "acquired_complete",
  "analysis_ready",
  "faib_complete",
  "recieved_complete",
  "received_complete"
)

get_latest_bc_status_file <- function(pattern = bc_status_pattern) {
  files <- Sys.glob(pattern)

  if (length(files) == 0) {
    stop("No Provincial_ALS_Status_*.shp files found in ", bc_source_dir)
  }

  status_files <- tibble(
    file = files,
    stamp = str_match(basename(files), "Provincial_ALS_Status_(\\d{8,12})\\.shp")[, 2]
  ) %>%
    filter(!is.na(stamp)) %>%
    arrange(stamp)

  if (nrow(status_files) == 0) {
    stop("No Provincial_ALS_Status_*.shp files with a parseable date stamp found")
  }

  status_files$file[nrow(status_files)]
}

bc_date_year <- function(x) {
  suppressWarnings(as.integer(substr(as.character(x), 1, 4)))
}

read_bc_status_layer <- function(file) {
  snapshot_stamp <- str_match(basename(file), "Provincial_ALS_Status_(\\d{8,12})\\.shp")[, 2]
  snapshot_year <- bc_date_year(snapshot_stamp)

  x <- st_read(file)

  if (!"status" %in% names(x)) {
    stop("Expected a `status` field in ", file)
  }

  x %>%
    filter(status %in% bc_status_available) %>%
    mutate(
      YEAR = coalesce(
        if ("enddate" %in% names(.)) bc_date_year(enddate) else NA_integer_,
        if ("startdate" %in% names(.)) bc_date_year(startdate) else NA_integer_,
        snapshot_year
      ),
      PPM = 8
    ) %>%
    filter(!is.na(YEAR)) %>%
    st_zm() %>%
    st_transform(crs = the_crs) %>%
    group_by(YEAR, PPM) %>%
    summarize(geometry = st_union(geometry), .groups = "drop")
}

# there were multiple shapefiles for BC
# first was sent by Chris Butson (Lidar_BC.shp) and had acquisitions starting in 2010 up to 2020
# second was downloaded from lidar.bc portal and had acquisitions starting in 2015 up to 2021
# also - there is a shapefile that Joanne found with 2014 acquisitions by Tolko
# on 2024-04-16 Chris Butson sent two more layers for 2022 and 2023 acquisitions
# on 2024-09-06 Chris Butson sent a layer with the most recent status of the lidar acquisitions
# on 2025-07-11 new layer sent

#those last two don't have any info on acquisition date or point density

#combing all of them here

ALS_BC_lidarbc <- st_read(
  "layers/source_layers/BC/BC_LiDAR_Point_Cloud_Index.shp"
) #last update 2024-04-09
ALS_BC_lidarbc %<>% select(YEAR = year_, PPM = density)
ALS_BC_lidarbc %<>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))

ALS_BC_older <- st_read("layers/source_layers/BC/Lidar_BC.shp")
ALS_BC_older %<>% select(YEAR, PPM = MinimumPts)
ALS_BC_older %<>% st_make_valid()
ALS_BC_older %<>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))
ALS_BC_older %<>% mutate(YEAR = as.numeric(YEAR))

ALS_BC_tolko <- st_read(
  "layers/source_layers/BC/Tolko_BCTS_LIDAR_acquisition.shp"
)
ALS_BC_tolko %<>%
  mutate(YEAR = 2014, PPM = 2) %>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))

## older ALS acquisitions (shared on 2025-11-26)
# North Vancouver Island 2012
BCTS_WFP_2012 <- st_read("layers/source_layers/BC/BCTS_WFP_2012.shp")
BCTS_WFP_2012 %<>%
  mutate(YEAR = 2012, PPM = 11) %>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))

# Cranbrook TSA 2011
Cranbrook_TSA_2011 <- st_read("layers/source_layers/BC/faib_ssc_lidar.shp")
Cranbrook_TSA_2011 %<>%
  mutate(YEAR = 2011, PPM = NA) %>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))

# Merritt TSA-2016
Merritt_TSA_2016 <- st_read(
  "layers/source_layers/BC/faib_merritt_lidar_2017.shp"
)
Merritt_TSA_2016 %<>%
  mutate(YEAR = 2016, PPM = NA) %>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))

# Sunshine Coast 2019?

ALS_BC_2022_bcts <- st_read("layers/source_layers/BC/bcts_lidar_2022.shp")
ALS_BC_2022_bcts %<>% select(YEAR = YearAcquir) %>% mutate(PPM = 8)

#Yes, so page 28, table 4 in the acquisition specs document:
#https://www2.gov.bc.ca/assets/gov/data/geographic/digital-imagery/specifications_for_airborne_lidar_for_the_province_of_british_columbia_53.pdf
#states the nominal last-return point densities for this program and they all have to meet Quality Level 2 (QL2) which is >=8pts/m2.
#Chris

ALS_BC_2022_bcts %<>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))

ALS_BC_2022 <- st_read(
  "layers/source_layers/BC/lidar_2022_se_july14_finalPriority12.shp"
)
ALS_BC_2022 %<>% mutate(YEAR = 2022, PPM = 8)
ALS_BC_2022 %<>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))

# new ALS acquisitions from the newest provincial status snapshot.
# When a new Provincial_ALS_Status_*.shp is added, this automatically uses it.
bc_status_file <- get_latest_bc_status_file()
message("Using BC provincial ALS status layer: ", basename(bc_status_file))
ALS_BC_status <- read_bc_status_layer(bc_status_file)

# st_write(ALS_BC_2025_no_overlap, dsn = "layers/BC/ALS_BC_2025_no_overlap.gpkg", append=F)
# plot(ALS_BC_2025_no_overlap)

# ALS_BC_union <- st_union(ALS_BC_lidarbc, ALS_BC_older)
# ALS_BC_union <- st_union(ALS_BC_union, ALS_BC_tolko)
# st_write(ALS_BC_union, dsn = "layers/BC/ALS_BC_union.gpkg")

ALS_BC <- bind_rows(
  BCTS_WFP_2012,
  Merritt_TSA_2016,
  Cranbrook_TSA_2011,
  ALS_BC_older,
  ALS_BC_lidarbc,
  ALS_BC_tolko,
  ALS_BC_2022_bcts,
  ALS_BC_2022,
  ALS_BC_status
)

#dissolve by year and PPM
ALS_BC <- ALS_BC %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry))


ALS_BC <- ALS_BC %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON", warn = FALSE) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry)) %>%
  mutate(Province = "BC") %>%
  mutate(isAvailable = 1) %>%
  relocate(Province, YEAR, PPM, area, isAvailable) %>%
  st_as_sf()

st_write(ALS_BC, dsn = "layers/pre-processed/BC/ALS_BC.gpkg", append = F)

# without overlaps - newest acquisition kept
ALS_BC_diss <- remove_overlaps_by_attr(ALS_BC, "YEAR")

#update area
ALS_BC_diss <- ALS_BC_diss %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON", warn = FALSE) %>%
  mutate(area = st_area(geometry))

st_write(
  ALS_BC_diss,
  dsn = "layers/pre-processed/BC/ALS_BC_diss.gpkg",
  append = F
)

# ALS_BC %<>%
#   st_as_sf() %>%
#   mutate(area = st_area(geometry)) %>%
#   mutate(Province="BC") %>%
#   mutate(isAvailable=1) %>%
#   relocate(Province, YEAR, PPM, area, isAvailable)
