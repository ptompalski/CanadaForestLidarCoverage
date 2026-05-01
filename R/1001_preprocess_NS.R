if (!exists("the_crs") || !exists("coverage_output_paths")) {
  source("R/0000_setup.R")
}

ns_source_file <- "layers/source_layers/NS/ALS_NS_downloaded_20231123.shp"
ns_output_paths <- coverage_output_paths("NS")

ALS_NS <- st_read(ns_source_file)

ALS_NS <-
  ALS_NS %>%
  select(YEAR = YEARDATE, PPM = PULSDENSRQ) %>%

  st_transform(crs = the_crs) %>%
  # st_make_valid() %>%
  mutate(area = st_area(.)) %>%
  group_by(YEAR, PPM) %>%
  summarise(n = n(), area = sum(area), geometry = st_union(geometry)) %>%
  assign_province_by_location() %>%
  group_by(Province, YEAR, PPM) %>%
  summarise(area = sum(st_area(geometry)), geometry = st_union(geometry), .groups = "drop") %>%
  mutate(isAvailable = 1) %>%
  add_source_metadata("NS GeoNOVA / NSGI", "Open point cloud and derivatives") %>%
  select(Province, YEAR, PPM, area, isAvailable, source_provider, source_access) %>%
  mutate(PPM = as.numeric(PPM))


st_write(
  ALS_NS,
  dsn = ns_output_paths$file,
  append = FALSE
)


# without overlaps - newest acquisition kept
ALS_NS_diss <- remove_overlaps_by_attr(ALS_NS, "YEAR")

#update area
ALS_NS_diss <- ALS_NS_diss %>% mutate(area = units::set_units(as.numeric(st_area(geometry)), m^2))

st_write(
  ALS_NS_diss,
  dsn = ns_output_paths$diss_file,
  append = F
)
