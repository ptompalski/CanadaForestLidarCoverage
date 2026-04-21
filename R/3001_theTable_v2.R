# purpose of this script is to calculate summary statistics needed for the
# "Acquisitions over time" figure on the first page:

# D <- readRDS("../5_lidarStatusCanada/ALS_coverage_all_2024.rds")
# D is loaded already in maps_setup
DD <- D %>% as_tibble() %>% dplyr::select(-geom)

D <- D %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON", warn = FALSE)

#calculate cumulative coverage up to each year ("what was the total coverage in 2010?")
years <- sort(unique(D$YEAR))

ALS_area_cumulative_by_year <- map_dfr(years, function(y) {
  D %>%
    filter(YEAR <= y) %>%
    group_by(Province) %>%
    summarize(.groups = "drop") %>% # dissolve by Province (handles overlap)
    mutate(total_area_dissolved = set_units(st_area(.), km^2), year = y) %>%
    select(
      jurisdiction_code = Province,
      year,
      total_ALS_area = total_area_dissolved
    ) %>%
    st_drop_geometry()
})

ALS_area_cumulative_by_year <- ALS_area_cumulative_by_year %>%
  arrange(jurisdiction_code, year)

ALS_coverage <-
  ALS_area_cumulative_by_year %>%
  left_join(provinces_area) %>%
  mutate(
    total_ALS_area_perc = as.numeric(total_ALS_area / total_area * 100),
    #cap at 100
    total_ALS_area_perc = if_else(
      total_ALS_area_perc > 100,
      100,
      total_ALS_area_perc
    )
  ) %>%
  relocate(total_ALS_area_perc)


# now calculating coverage in the managed forests
# coverageManagedUnmanaged <- readRDS("layers/coverageManagedUnmanaged.rds")
manage_unmanaged_v2 <- rast(
  "//vic-fas1/france/cwfc/forest_inventory_status/spatial_data/managed_forest_mask.tif"
)

# replace NA with a value (to compare total areas of all classes to areas of jurisdictions)
manage_unmanaged_v2 <- subst(manage_unmanaged_v2, NA, 3)

# # how many pixels of each class within each province
# # this doesn't change over time so is calculated only once
# EXTR_prov <- terra::extract(manage_unmanaged_v2, provinces, fun = table)

# # how many pixels of each class within each acquisition area, by year (cumulative)
# y <- 2010

# D_current <-
#   D %>%
#   filter(YEAR <= y) %>%
#   group_by(Province) %>%
#   summarize(.groups = "drop")

# EXTR <- terra::extract(manage_unmanaged_v2, D_current, fun = table)

# ---- 3) Loop over years, dissolve by Province (≤ y), extract summed area by class ----
ALS_area_cumulative_by_year_class <- map_dfr(years, function(y) {
  # dissolve all polys with YEAR ≤ y by Province (avoid double counting overlaps)
  D_current <- D %>%
    filter(YEAR <= y) %>%
    group_by(Province) %>%
    summarize(.groups = "drop")

  # convert to SpatVector for terra::extract
  D_spat <- terra::vect(D_current)

  # extract summed area (km²) per class; bind=TRUE keeps Province attribute
  ex <- terra::extract(
    manage_unmanaged_v2,
    D_spat,
    fun = table,
    na.rm = TRUE,
    bind = TRUE
  )

  ex$Province <- D_current$Province

  # tidy to long format: Province × year × class → area_km2
  tibble::as_tibble(ex) %>%
    rename(jurisdiction_code = Province) %>%
    mutate(year = y) %>%
    select(jurisdiction_code, year, `1`:`3`) %>%
    pivot_longer(`1`:`3`, names_to = "class", values_to = "area_km2")
})

#pivot wider to make it one row per year
ALS_area_cumulative_by_year_class <-
  ALS_area_cumulative_by_year_class %>%
  pivot_wider(names_from = class, values_from = area_km2)

ALS_area_cumulative_by_year_class <-
  ALS_area_cumulative_by_year_class %>%
  rename(
    managed_ALS_area = `1`,
    unmanaged_ALS_area = `2`,
    nonForest_ALS_area = `3`
  ) %>%
  mutate(across(ends_with("area"), ~ 250 * 250 * .x / 10000))


# #calculate coverage in the managed forests

ALS_coverage_overall <- readRDS(
  file = "layers/coverageManagedUnmanaged.rds"
) %>%
  filter(forest_type == "managed") %>%
  rename(jurisdiction_code = jurisdiction)

ALS_area_cumulative_by_year_class %>%
  select(jurisdiction_code, year, managed_ALS_area) %>%
  left_join(ALS_coverage_overall) %>%
  arrange(jurisdiction_code, year) %>%

  mutate(prop_yearly = managed_ALS_area / area_prov * 100) %>%
  mutate(prop_yearly = if_else(prop_yearly > 100, 100, prop_yearly))


# combine into one df

ALS_coverage <-
  ALS_coverage %>%
  left_join(ALS_area_cumulative_by_year_class)

ALS_coverage <-
  ALS_coverage %>%
  rename(
    total_ALS_coverage_area = total_ALS_area,
    total_ALS_coverage_area_perc = total_ALS_area_perc,
    managed_ALS_coverage_area = managed_ALS_area,
    unmanaged_ALS_coverage_area = unmanaged_ALS_area,
    nonForest_ALS_coverage_area = nonForest_ALS_area
  )

saveRDS(ALS_coverage, "layers/coverageOverTime.rds")


#
dataForTheFigure <- readRDS("layers/dataForTheFigure.rds")

ALS_coverage_orig <- dataForTheFigure$DF_area

ALS_coverage_orig <-
  ALS_coverage_orig %>%
  rename(
    jurisdiction_code = Jurisdiction,
    year = YEAR2,
    Total_ALS_acquisitions_area = Area_km2,
    Total_ALS_acquisitions_area_cumulative = Area_km2_cumulative
  ) %>%
  select(
    jurisdiction_code,
    year,
    Total_ALS_acquisitions_area,
    Total_ALS_acquisitions_area_cumulative
  )

# needed:
# - ALS coverage per province in km2                    ("total_ALS_coverage_area")
# - ALS coverage per province in %                      ("total_ALS_coverage_area_perc")
# - ALS coverage per province, managed forest only, km2 ("managed_ALS_coverage_area")
# - ALS coverage per province, managed forest only, %   ("managed_ALS_coverage_area_perc")

# - ALS cumulative acquisition area per province in km2  ("Total_ALS_acquisitions_area")
# - ALS cumulative acquisition area per province in %    ("Total_ALS_acquisitions_area_cumulative")
# - ALS cumulative acquisition area per province, managed forest only, km2 ("")
# - ALS cumulative acquisition area per province, managed forest only, %   ("")

A <-
  ALS_coverage_orig %>%
  left_join(ALS_coverage, by = join_by(jurisdiction_code, year)) %>%
  mutate(
    managed_ALS_coverage_area_perc = (managed_ALS_coverage_area * 0.01) /
      managed_forest_area *
      100
  )

A <- A %>%
  mutate(
    tip = glue::glue(
      "{jurisdiction_code}\n{year}\nP<sub>J</sub>={round(total_ALS_coverage_area_perc,1)}%\nP<sub>M</sub>={round(managed_ALS_coverage_area_perc,1)}%"
    )
  )
saveRDS(A, "layers/dataForTheFigure_v3.rds")
