# purpose of this script is to calculate summary statistics about the multitemporal coverage

coverage_managed_unmanaged_file <- Sys.getenv(
  "COVERAGE_MANAGED_UNMANAGED_FILE",
  unset = "layers/coverageManagedUnmanaged.rds"
)
summary_multitemporal_file <- Sys.getenv(
  "SUMMARY_MULTITEMPORAL_FILE",
  unset = "layers/summary_multitemporal_list.rds"
)

#### BY JURISDICTION ####

# load data - should be loaded with the 2000_maps_setup.R
O1 <- O %>%
  group_by(Province) %>%

  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON", warn = FALSE) %>%
  summarize()

multitemporal_ALS_area_dissolved_by_province <-
  O1 %>%
  mutate(multitemporal_ALS_area = set_units(st_area(.), km^2)) %>%
  select(
    jurisdiction_code = Province,
    multitemporal_ALS_area
  ) %>%
  as_tibble() %>%
  select(-geom)


total_multitemporal_ALS_area <- sum(
  multitemporal_ALS_area_dissolved_by_province$multitemporal_ALS_area
)

summary_multitemporal <-
  multitemporal_ALS_area_dissolved_by_province %>%
  left_join(provinces_area) %>%
  mutate(
    multitemporal_ALS_area_prop = as.numeric(
      multitemporal_ALS_area / total_area * 100
    )
  )


#### MANAGED FORESTS ONLY #####

# coverage limited to the managed forests
coverageManagedUnmanaged <- readRDS(coverage_managed_unmanaged_file)


manage_unmanaged_v2 <- rast(managed_forest_mask_path)

# replace NA with a value (to compare total areas of all classes to areas of jurisdictions)
manage_unmanaged_v2 <- subst(manage_unmanaged_v2, NA, 3)

ex_O1 <- terra::extract(
  manage_unmanaged_v2,
  vect(O1),
  fun = table,
  na.rm = TRUE,
  bind = TRUE
)


ex_O1$Province <- O1$Province

# tidy to long format: Province × year × class → area_km2
ex_O1 <- tibble::as_tibble(ex_O1) %>%
  rename(jurisdiction_code = Province) %>%
  # mutate(year = y) %>%
  select(jurisdiction_code, `1`:`3`) %>%
  pivot_longer(`1`:`3`, names_to = "class", values_to = "area_km2")


#pivot wider to make it one row per year
managed_multitemporal_ALS_area <-
  ex_O1 %>%
  pivot_wider(names_from = class, values_from = area_km2) %>%
  rename(
    managed_multitemporal_ALS_area = `1`,
    unmanaged_multitemporal_ALS_area = `2`,
    nonForest_multitemporal_ALS_area = `3`
  ) %>%
  mutate(across(ends_with("area"), ~ 250 * 250 * .x / 10000)) %>%
  select(jurisdiction_code, managed_multitemporal_ALS_area)


#get province totals - total managed area per jurisdiction
managed_area_byProvince <- readRDS(
  file = coverage_managed_unmanaged_file
) %>%
  filter(forest_type == "managed") %>%
  select(jurisdiction_code = jurisdiction, area_prov)

summary_multitemporal2 <-
  managed_multitemporal_ALS_area %>%
  left_join(managed_area_byProvince) %>%
  mutate(
    managed_multitemporal_ALS_area_prop = managed_multitemporal_ALS_area /
      area_prov *
      100
  )


#### final summary #####

#overall summary for entire Canada
summary_multitemporal_Canada <-
  summary_multitemporal %>%
  summarise(
    total_multitemporal_ALS_area = sum(multitemporal_ALS_area),
    total_area = sum(total_area)
  ) %>%
  mutate(
    total_multitemporal_ALS_area_prop = as.numeric(
      total_multitemporal_ALS_area / total_area * 100
    )
  )

#overall summary for all of managed forests
summary_multitemporal_managed <-
  summary_multitemporal2 %>%
  summarise(
    total_managed_multitemporal_ALS_area = sum(managed_multitemporal_ALS_area),
    total_area = sum(area_prov)
  ) %>%
  mutate(
    total_managed_multitemporal_ALS_area_prop = as.numeric(
      total_managed_multitemporal_ALS_area / total_area * 100
    )
  )

#summary by province, including managed forests
summary_multitemporal_byProvince <-
  summary_multitemporal %>%
  left_join(summary_multitemporal2) %>%
  select(
    jurisdiction_name,
    jurisdiction_code,
    multitemporal_ALS_area,
    multitemporal_ALS_area_prop,
    managed_multitemporal_ALS_area,
    managed_multitemporal_ALS_area_prop
  )

summary_multitemporal_list <- list(
  summary_multitemporal_Canada = summary_multitemporal_Canada,
  summary_multitemporal_managed = summary_multitemporal_managed,
  summary_multitemporal_byProvince = summary_multitemporal_byProvince
)
saveRDS(summary_multitemporal_list, summary_multitemporal_file)
