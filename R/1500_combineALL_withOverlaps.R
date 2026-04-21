#identify areas of multiple acquitions

if (!exists("the_crs") || !exists("read_preprocessed_coverage")) {
  source("R/0000_setup.R")
}

# load layers for every jurisdiction
# these should be layers that are dissolved by year + PPM,
# and should include overlaps

jurisdictions <- c("AB", "BC", "NB", "ON", "QC", "PEI", "NS", "SK")
coverage_layers <- read_preprocessed_coverage(
  jurisdictions,
  path_overrides = c(
    # using the dissolved layer here because of the errors in the coverage
    # shapefiles (duplicated acquisitions)
    ON = preprocessed_coverage_path("ON", dissolved = TRUE)
  )
)

M <- do.call(rbind, coverage_layers)
provinces2 <- read_province_boundaries()
M <- st_intersection(M, provinces2)

#classify point density
M$PPM_class <- cut(
  M$PPM,
  breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
  labels = c("<1", "1-2", "2-5", "5-10", "10-20", "20-50", ">50")
)

# save
st_write(
  M,
  dsn = glue(
    "layers/ALS_coverage_layer/multitemporal/ALS_coverage_multitemporal_{ver}.gpkg"
  ),
  append = F
)


# dissolve by year and jurisdiction
D_YEARs <- M %>%
  group_by(Province, YEAR) %>%
  summarize(geometry = st_union(geom))


# identify overlaping areas ####

all_years <- sort(unique(as.numeric(D_YEARs$YEAR)))

year_combinations <- expand_grid(YEAR1 = all_years, YEAR2 = all_years) %>%
  filter(YEAR1 != YEAR2)

#a trick to remove reduntand combinations
year_combinations <- year_combinations[
  year_combinations$YEAR1 >= year_combinations$YEAR2,
]


O <- O1 <- list()


for (i in 1:nrow(year_combinations)) {
  current_combination <- year_combinations[i, ]

  year1 <- D_YEARs %>% filter(YEAR == current_combination$YEAR1)
  year2 <- D_YEARs %>% filter(YEAR == current_combination$YEAR2)

  int <- st_intersection(year1, year2)

  if (nrow(int) > 0) {
    if (any(st_geometry_type(int) != "MULTILINESTRING")) {
      int <- st_collection_extract(int)
      int <- int %>% st_cast("POLYGON")

      O[[i]] <- int
    }
  }
}

O <- bind_rows(O)

# make year columns make sense
# call them YEAR1 (for the smaller value) and YEAR2 (larger)
O <- O %>%
  mutate(
    YEAR1 = pmin(YEAR, YEAR.1, na.rm = TRUE),
    YEAR2 = pmax(YEAR, YEAR.1, na.rm = TRUE)
  ) %>%
  select(
    -YEAR,
    -YEAR.1,
    -Province.1
  )


#intersect with provinces
# provinces <- st_read(file.path(PATH,"useful_layers", "ca_prov.shp"))
# provinces %<>% select(PROV)
#
# O <- st_intersection(O, provinces)

#calculate difference in acquisition years
O %<>%
  mutate(across(c(YEAR1, YEAR2), as.numeric)) %>%
  mutate(YEAR_delta = abs(YEAR1 - YEAR2))

# O <- O %>% st_cast("POLYGON")

#calcuate area
O %<>% mutate(area_ha = as.numeric(st_area(geometry)) / 10000)


#remove polys smaller than X ha
O %<>% filter(area_ha > 100)

# at least 3 years between acquisitions
O <- O %>% filter(YEAR_delta > 2)

# clean weird linear features
O <- drop_slivers_buffer(O, 1000, FALSE)


st_write(
  O,
  dsn = glue(
    "layers/ALS_coverage_layer/overlap/ALS_coverage_overlap_{ver}.gpkg"
  ),
  append = F
)
