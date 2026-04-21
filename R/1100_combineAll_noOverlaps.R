if (!exists("the_crs") || !exists("read_preprocessed_coverage")) {
  source("R/0000_setup.R")
}

#availability codes
# 0 - not available (most of AB)
# 1 - all data available, easy access through a web portal (e.g. BC, NS)
# 2 - all data available but access is complicated, no web portal etc (ON)
# 3 - only raster products are available (DTM, DSM etc) QC?

coverage_main_file <- Sys.getenv(
  "COVERAGE_MAIN_FILE",
  unset = glue("layers/ALS_coverage_layer/main/ALS_coverage_all_{ver}.rds")
)
coverage_clipped_file <- Sys.getenv(
  "COVERAGE_CLIPPED_FILE",
  unset = "layers/ALS_coverage_all_2025_clipped.rds"
)
coverage_generalized_file <- Sys.getenv(
  "COVERAGE_GENERALIZED_FILE",
  unset = glue(
    "layers/ALS_coverage_layer/generalized/ALS_coverage_all_{ver}_generalized_v2.gpkg"
  )
)

dir_create(dirname(coverage_main_file))
dir_create(dirname(coverage_clipped_file))
dir_create(dirname(coverage_generalized_file))

jurisdictions <- c("AB", "BC", "NB", "ON", "QC", "PEI", "NS", "SK")
coverage_diss <- read_preprocessed_coverage(jurisdictions, dissolved = TRUE)


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

D <- do.call(rbind, coverage_diss)

#make sure year is a number
D$YEAR <- as.numeric(D$YEAR)

## CLIP TO FORESTED ECOZONEZES ##
#new step added on 2024-09-2024
D <- clip_to_forested_ecozones(D)

saveRDS(D, coverage_main_file)

# st_write(D, glue("ALS_coverage_all_{ver}.gpkg"), append=F)

#postprocess the acquisition polygons

# clip to provinces vector (no areas outside)
provinces2 <- read_province_boundaries()
Dx <- st_intersection(D, provinces2)
saveRDS(Dx, coverage_clipped_file)

# dissolve all to create a single poly
D1 <- Dx %>% ungroup() %>% summarise(n = n()) %>% st_cast()

#remove holes
# library(nngeo)
D1 <- nngeo::st_remove_holes(D1, max_area = 2500000)

D1 <- rmapshaper::ms_simplify(D1, keep = 0.02)

# st_write(D1, glue("ALS_coverage_all_{ver}_generalized_v2.gpkg"), append=F)
st_write(
  D1,
  coverage_generalized_file,
  append = F
)
