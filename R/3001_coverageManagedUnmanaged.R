#how much ALS by managed/unmanaged by province

# data
manage_unmanaged_v2 <- rast(
  "//vic-fas1/france/cwfc/forest_inventory_status/spatial_data/managed_forest_mask.tif"
)
provinces <- vect(file.path(
  "K:/OneDrive - NRCan RNCan/_WORK",
  "useful_layers",
  "Canada_provinces.shp"
))
# D <- vect("ALS_coverage_all_2024.gpkg") #already loaded

D <- D %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON", warn = FALSE)


# group acquisitions by jurisdiction
D_jurisdiction <- terra::aggregate(
  x = vect(D),
  by = "Province",
  dissolve = TRUE
)

# replace NA with a value (to compare total areas of all classes to areas of jurisdictions)
manage_unmanaged_v2 <- subst(manage_unmanaged_v2, NA, 3)

# how many pixels of each class within each acquisition area
EXTR <- terra::extract(manage_unmanaged_v2, D_jurisdiction, fun = table)


# how many pixels of each class within each province
EXTR_prov <- terra::extract(manage_unmanaged_v2, provinces, fun = table)


EXTR


EXTR$jurisdiction <- D_jurisdiction$Province
EXTR_prov$jurisdiction <- toupper(provinces$PROV)

x1 <- EXTR_prov %>%
  dplyr::select(-ID) %>%
  pivot_longer(-jurisdiction, names_to = "class", values_to = "area_prov")

x2 <- EXTR %>%
  dplyr::select(-ID) %>%
  pivot_longer(-jurisdiction, names_to = "class", values_to = "area_ALS")

X <- left_join(x1, x2)

X <- X %>% mutate(across(starts_with("area_"), ~ 250 * 250 * .x / 10000))

X <- X %>%
  mutate(prop = area_ALS / area_prov * 100) %>%
  # filter(!is.na(area_ALS), class != "3") %>%
  # mutate(forest_type = if_else(class=="1", "managed", "unmanaged")) %>%
  mutate(
    forest_type = case_when(
      class == "1" ~ "managed",
      class == "2" ~ "unmanaged",
      class == "3" ~ "notForest",
    )
  ) %>%

  dplyr::select(-class) %>%
  relocate(forest_type, .after = jurisdiction)

#check
X %>%
  group_by(jurisdiction) %>%
  summarise(
    area_prov = sum(area_prov, na.rm = T),
    area_ALS = sum(area_ALS, na.rm = T)
  )


#cap the proportions at 100%
X <- X %>% mutate(prop = if_else(prop > 100, 100, prop))

saveRDS(X, file = "layers/coverageManagedUnmanaged.rds")


manage_unmanaged_byJurisdiction <-
  x1 %>%
  mutate(across(starts_with("area_"), ~ 250 * 250 * .x / 10000)) %>%
  mutate(
    forest_type = case_when(
      class == "1" ~ "managed",
      class == "2" ~ "unmanaged",
      class == "3" ~ "notForest",
    )
  ) %>%
  dplyr::select(-class) %>%
  relocate(forest_type, .after = jurisdiction)
saveRDS(
  manage_unmanaged_byJurisdiction,
  "layers/manage_unmanaged_byJurisdiction.rds"
)

# #save managed mask as vector?
# # 1) Keep only the pixels == 1
# r1 <- manage_unmanaged_v2 == 1  # logical SpatRaster: TRUE for 1, FALSE otherwise

# # 2) Polygonize; dissolve merges touching TRUE cells into larger polygons
# pol_raw <- as.polygons(r1, dissolve = TRUE)

# # 3) Keep only the TRUE polygons (value==1)
# # pol_1 <- pol_raw[ pol_raw$lyr.1 == 1, ]
# #

# pol_raw <- st_as_sf(pol_raw)

# pol_raw_dissolved <- pol_raw |>
#   st_union() |>
#   st_make_valid() |>
#   st_as_sf()
