if (!exists("the_crs") || !exists("coverage_output_paths")) {
  source("R/0000_setup.R")
}

pei_output_paths <- coverage_output_paths("PEI")

ALS_PEI <- st_read(
  "layers/source_layers/PE/pei_2020_1k_grid_LIDAR_Coverage.shp"
)

ALS_PEI_2020 <-
  ALS_PEI %>%
  mutate(YEAR = 2020, PPM = 6) %>%
  st_transform(crs = the_crs) %>%
  st_make_valid() %>%
  mutate(area = st_area(.)) %>%
  group_by(YEAR, PPM) %>%
  summarise(n = n(), area = sum(area), geometry = st_union(geometry)) %>%
  mutate(Province = "PE") %>%
  mutate(isAvailable = 1) %>%
  st_cast() %>%
  select(Province, YEAR, PPM, area, isAvailable)

# Hey Adam,
# I’ve answered the LiDAR questions below and included a shapefile of the completed LiDAR Acquisition. Our EFI should be delivered at the end of the month.
# - Year of acquisition: Summer 2020
# - Acquisition point density: >=6ppm
# - Acquisition technology SPL or traditional capture: traditional
# - Data availability: point cloud or surfaces available through open data portals, including NRCAN HRDEM portal: surfaces via NRCAN HRDEM portal
# - Data ownership: acquired as part of a provincial program or areas that were privately acquired by forest companies or other industries: Provincial
# Thanks,
# Andrew

#adding the 2010 coverage
#"PEI had full coverage in 2010, but it was leaf off. Apparently, they did make an EFI out of it (Chris actually did the modeling). Matt did not mention this...so the question is how to capture that in the figure? 100% coverage in 2010 and 100% in 2020?
ALS_PEI_2010 <- st_read(
  "layers/source_layers/PE/pei_2010_fullCoverageBasedOnInfoFromChrisHennigar.shp"
)
st_crs(ALS_PEI_2010) <- st_crs(ALS_PEI)
ALS_PEI_2010 %<>% st_transform(crs = the_crs)

ALS_PEI_2010 %<>%
  mutate(
    Province = "PE",
    YEAR = 2010,
    PPM = 1, #PPM is unknown
    # n = NA,
    area = set_units(st_area(.), m^2),
    isAvailable = 0
  ) %>%
  select(-(ENCLOSED_A:NORTH))


ALS_PEI <- bind_rows(
  ALS_PEI_2010,
  ALS_PEI_2020
)

#dissolve by year and PPM
ALS_PEI <- ALS_PEI %>% dissolve_coverage()


ALS_PEI <- ALS_PEI %>% finalize_available_coverage("PE")

st_write(ALS_PEI, dsn = pei_output_paths$file, append = F)

# without overlaps - newest acquisition kept
ALS_PEI_diss <- remove_overlaps_by_attr(ALS_PEI, "YEAR")

#update area
ALS_PEI_diss <- ALS_PEI_diss %>% mutate(area = st_area(geometry))

st_write(
  ALS_PEI_diss,
  dsn = pei_output_paths$diss_file,
  append = F
)
