#identify areas of multiple acquitions

source("0000_setup.R")

#directory with coverage files
PATH <- "layers/ALS_coverage_layer/"

# list all files, with modification time
flist <- dir_info(file.path(PATH, "main"), recurse = FALSE)

# get the newest - this file will be used in almost all processing
f <- flist %>% arrange(desc(modification_time)) %>% slice(1) %>% pull(path)

# ALS coverage
D <- readRDS(f)


D_YEARs <- D %>%
  group_by(Province, YEAR) %>%
  summarize(geometry = st_union(geometry))


all_years <- unique(as.numeric(D_YEARs$YEAR))

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

  #second intersect, after negative buffer
  # year1a <- D_YEARs_shrunk %>% filter(YEAR==current_combination$YEAR1)
  # year2a <- D_YEARs_shrunk %>% filter(YEAR==current_combination$YEAR2)
  # int2 <- st_intersection(year1a, year2a)

  if (nrow(int) > 0) {
    if (any(st_geometry_type(int) != "MULTILINESTRING")) {
      int <- st_collection_extract(int)
      int <- int %>% st_cast("POLYGON")

      O[[i]] <- int

      # int2 <- st_collection_extract(int2)
      # int2 <- int2 %>% st_cast("POLYGON")

      # O1[[i]] <-   int2
    }
  }
}

O <- bind_rows(O)
# O1 <-bind_rows(O1)

# st_write(O1, dsn = "ALS_coverage_multitemporal_shrunk.gpkg", append=F)

# post-processing - data needs to be filtered
#1. remove polys that are too small
#2. remove polys by province - we know that there were second acquisitions in e.g. ON
#3. remove polys that have difference in acquisiiton year less than 1 or 2 years

# O <- st_read( "ALS_coverage_multitemporal.gpkg")
# O1 <- st_read( "ALS_coverage_multitemporal_shrunk.gpkg")

#intersect with provinces
# provinces <- st_read(file.path(PATH,"useful_layers", "ca_prov.shp"))
# provinces %<>% select(PROV)
#
# O <- st_intersection(O, provinces)

#calculate difference in acquisition years
O %<>%
  mutate(across(c(YEAR, YEAR.1), as.numeric)) %>%
  mutate(YEAR_delta = abs(YEAR - YEAR.1))

# O <- O %>% st_cast("POLYGON")

#calcuate area
O %<>% mutate(area_ha = as.numeric(st_area(geometry)) / 10000)
# st_write(O1, dsn = "ALS_coverage_multitemporal_shrunk.gpkg", append=F)

#remove polys smaller than X ha
O %<>% filter(area_ha > 100)

# st_write(O, dsn = "ALS_coverage_multitemporal_v2.gpkg", append = F)
st_write(
  O,
  dsn = glue(
    "layers/ALS_coverage_layer/multitemporal/ALS_coverage_multitemporal_{ver}.gpkg"
  ),
  append = F
)

# plot(O["YEAR_delta"])

# remove polys if the acquisition years are 2 or less years apart
# O1 %<>% filter(YEAR_delta > 2)
# st_write(O1, dsn = "ALS_coverage_multitemporal_shrunk2.gpkg", append=F)

#next step - select by location - is done in QGIS manually
# this is because the results weren't correct if
# similar operation was done in R with the code below

# #overlay the shrunk polys with the original ones
# is_overlap <- st_overlaps(O, O1)
#
# # is_overlap <- st_intersects(O, O1)
# is_overlap <- is_overlap%>% lengths > 0
#
# # int3 <- st_intersection(O, O1)
#
# # plot(st_geometry(O[1,]))
# # plot(st_geometry(O[2,]))
# # plot(st_geometry(O))
#
# O <- O[is_overlap,]
# st_write(O, dsn = "ALS_coverage_multitemporal_overlap.gpkg", append = F)

# POSTPROCESS

# ALS_coverage_multitemporal <- st_read("ALS_coverage_multitemporal_manual.gpkg")
# ALS_coverage_multitemporal <- ALS_coverage_multitemporal %>% group_by(YEAR, YEAR.1) %>%
#   summarise(geometry = st_union(geom))
#
#
# ALS_coverage_multitemporal %<>% mutate(deltaYears = as.numeric(YEAR) - as.numeric(YEAR.1))
#
#
# plot(ALS_coverage_multitemporal["deltaYears"])
