nb_output_dir <- Sys.getenv("NB_OUTPUT_DIR", unset = "layers/pre-processed/NB")
nb_output_file <- Sys.getenv(
  "NB_OUTPUT_FILE",
  unset = file.path(nb_output_dir, "ALS_NB.gpkg")
)
nb_output_diss_file <- Sys.getenv(
  "NB_OUTPUT_DISS_FILE",
  unset = file.path(nb_output_dir, "ALS_NB_diss.gpkg")
)

dir_create(dirname(nb_output_file))
dir_create(dirname(nb_output_diss_file))

ALS_NB_1 <- st_read("layers/source_layers/NB/geonb_li_idl_cgvd2013.shp")
ALS_NB_2 <- st_read("layers/source_layers/NB/geonb_li_idl_cgvd1928.shp")

ALS_NB_1 <- ALS_NB_1 %>% st_transform(crs = the_crs)
ALS_NB_2 <- ALS_NB_2 %>% st_transform(crs = the_crs)

ALS_NB <- bind_rows(ALS_NB_1, ALS_NB_2)


#correct acquisition year
ALS_NB <-
  ALS_NB %>%
  mutate(
    Year = str_replace(pattern = "2017&2018", replacement = "2017", Year)
  ) %>%
  mutate(Year = str_replace(pattern = "20172018", replacement = "2017", Year))


ALS_NB %<>% select(YEAR = Year, PPM)
ALS_NB %<>% mutate(YEAR = as.numeric(str_sub(YEAR, start = 1, end = 4)))

#dissolve by year and PPM
ALS_NB <- ALS_NB %>% dissolve_coverage(make_valid = TRUE)


ALS_NB <- ALS_NB %>%
  st_make_valid() %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry)) %>%
  mutate(Province = "NB") %>%
  mutate(isAvailable = 1) %>%
  relocate(Province, YEAR, PPM, area, isAvailable) %>%
  st_as_sf()

st_write(ALS_NB, dsn = nb_output_file, append = F)

# without overlaps - newest acquisition kept
ALS_NB_diss <- remove_overlaps_by_attr(ALS_NB, "YEAR")

#update area
ALS_NB_diss <- ALS_NB_diss %>% mutate(area = st_area(geometry))

st_write(
  ALS_NB_diss,
  dsn = nb_output_diss_file,
  append = F
)

# ALS_NB_diss <-
#   ALS_NB %>%
#   group_by(YEAR, PPM) %>%
#   summarise(n = n(), area = sum(area), geometry = st_union(geometry)) %>%
#   mutate(Province = "NB") %>%
#   mutate(isAvailable = 1) %>%
#   st_cast() %>%
#   select(Province, YEAR, PPM, n, area, isAvailable)
