# prepare data for figure showing acquisition area over time
# this figure could show:
# a) total or cumulative area of acquisitions by year, including overlaps (overlaps counted)
# b) total or cumulative area of acquisitions by year, but excluding overlaps

# using data with acquisitions by year (not dissolved)
coverage_managed_unmanaged_file <- Sys.getenv(
  "COVERAGE_MANAGED_UNMANAGED_FILE",
  unset = "layers/coverageManagedUnmanaged.rds"
)
data_for_figure_v4_file <- Sys.getenv(
  "DATA_FOR_FIGURE_V4_FILE",
  unset = "layers/dataForTheFigure_v4.rds"
)
if (!exists("PATH")) {
  PATH <- "layers/ALS_coverage_layer/"
}
f2 <- Sys.getenv(
  "MULTITEMPORAL_OUTPUT_FILE",
  unset = latest_file_by_pattern(
    file.path(PATH, "multitemporal/ALS_coverage_multitemporal_*.gpkg"),
    stamp_regex = "ALS_coverage_multitemporal_(\\d{8})\\.gpkg",
    label = "multitemporal ALS coverage GPKG"
  )
)
Q <- st_read(f2)

Q <- Q %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON", warn = FALSE)

Q$YEAR <- as.numeric(Q$YEAR)
years <- sort(unique(Q$YEAR))


# calculate coverage up to each year at jurisdiction level
# ("what was the total coverage in 2010?")

ALS_coverage_by_year <- map_dfr(years, function(y) {
  Q %>%
    filter(YEAR <= y) %>%
    group_by(Province) %>%
    summarize(.groups = "drop") %>% # dissolve by Province (handles overlap)
    mutate(total_area_dissolved = set_units(st_area(.), km^2), year = y) %>%
    select(
      jurisdiction_code = Province,
      year,
      total_ALS_coverage_area = total_area_dissolved
    ) %>%
    st_drop_geometry()
}) %>%
  arrange(jurisdiction_code, year)

## calculate coverage proportion

ALS_coverage <-
  ALS_coverage_by_year %>%
  left_join(provinces_area %>% select(jurisdiction_code, total_area)) %>%
  mutate(
    total_ALS_coverage_area_perc = as.numeric(
      total_ALS_coverage_area / total_area * 100
    ),
    #cap at 100
    total_ALS_coverage_area_perc = if_else(
      total_ALS_coverage_area_perc > 100,
      100,
      total_ALS_coverage_area_perc
    )
  )


# summarize total acquisition area by year by province
ALS_area_by_year <- Q %>%
  mutate(area = set_units(st_area(.), km^2)) %>%
  as_tibble() %>%
  group_by(jurisdiction_code = Province, year = YEAR) %>%
  summarise(total_acquisition_area = sum(area)) %>%
  # add cumulative area
  group_by(jurisdiction_code) %>%
  mutate(cumulative_total_acquisition_area = cumsum(total_acquisition_area))

ALS_coverage <-
  ALS_coverage %>%
  left_join(ALS_area_by_year)


# managed forests ####
# now calculating coverage in the managed forests

manage_unmanaged_v2 <- rast(managed_forest_mask_path)

# replace NA with a value (to compare total areas of all classes to areas of jurisdictions)
manage_unmanaged_v2 <- subst(manage_unmanaged_v2, NA, 3)

province_lookup <- tibble(
  jurisdiction_code = sort(unique(Q$Province)),
  province_id = seq_along(jurisdiction_code)
)

Q_raster <- Q %>%
  dplyr::left_join(province_lookup, by = c("Province" = "jurisdiction_code"))

r_first_year <- terra::rasterize(
  terra::vect(Q_raster),
  manage_unmanaged_v2,
  field = "YEAR",
  fun = "min"
)
r_province <- terra::rasterize(
  terra::vect(Q_raster),
  manage_unmanaged_v2,
  field = "province_id",
  fun = "min"
)

names(r_first_year) <- "year"
names(r_province) <- "province_id"
names(manage_unmanaged_v2) <- "class"

coverage_cells <- terra::crosstab(
  c(r_province, r_first_year, manage_unmanaged_v2),
  long = TRUE,
  useNA = FALSE
)
names(coverage_cells)[names(coverage_cells) %in% c("Freq", "freq")] <- "n_cells"
names(coverage_cells)[names(coverage_cells) == "n"] <- "n_cells"

ALS_coverage_by_year_class_dissolved <-
  coverage_cells %>%
  tibble::as_tibble() %>%
  dplyr::left_join(province_lookup, by = "province_id") %>%
  dplyr::mutate(
    year = as.numeric(year),
    class = as.character(class),
    area_km2 = n_cells
  ) %>%
  dplyr::select(jurisdiction_code, year, class, area_km2) %>%
  tidyr::complete(
    jurisdiction_code,
    year = years,
    class,
    fill = list(area_km2 = 0)
  ) %>%
  dplyr::arrange(jurisdiction_code, class, year) %>%
  dplyr::group_by(jurisdiction_code, class) %>%
  dplyr::mutate(area_km2 = cumsum(area_km2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(dissolved = TRUE)

Q_raw <- Q %>%
  dplyr::mutate(poly_id = dplyr::row_number())

ex_raw <- terra::extract(
  manage_unmanaged_v2,
  terra::vect(Q_raw),
  fun = table,
  na.rm = TRUE,
  bind = TRUE
)

ex_raw$Province <- Q_raw$Province
ex_raw$YEAR <- Q_raw$YEAR

ALS_coverage_by_year_class_total <-
  ex_raw %>%
  tibble::as_tibble() %>%
  dplyr::rename(jurisdiction_code = Province, year = YEAR) %>%
  dplyr::select(jurisdiction_code, year, `1`:`3`) %>%
  tidyr::pivot_longer(`1`:`3`, names_to = "class", values_to = "area_km2") %>%
  dplyr::group_by(jurisdiction_code, year, class) %>%
  dplyr::summarize(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(
    jurisdiction_code,
    year = years,
    class,
    fill = list(area_km2 = 0)
  ) %>%
  dplyr::arrange(jurisdiction_code, class, year) %>%
  dplyr::group_by(jurisdiction_code, class) %>%
  dplyr::mutate(area_km2 = cumsum(area_km2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(dissolved = FALSE)

ALS_coverage_by_year_class <- dplyr::bind_rows(
  ALS_coverage_by_year_class_dissolved,
  ALS_coverage_by_year_class_total
)


#pivot wider to make it one row per year
ALS_coverage_by_year_class2 <-
  ALS_coverage_by_year_class %>%
  pivot_wider(names_from = class, values_from = area_km2) %>%
  rename(
    managed_ALS_area = `1`,
    unmanaged_ALS_area = `2`,
    nonForest_ALS_area = `3`
  ) %>%
  mutate(across(ends_with("area"), ~ 250 * 250 * .x / 10000)) %>%
  mutate(dissolved = if_else(dissolved == TRUE, "coverage", "total")) %>%

  pivot_wider(names_from = "dissolved", values_from = ends_with("area"))


# calculate proportions in the managed forests

managed_area_byProvince <- readRDS(
  file = coverage_managed_unmanaged_file
) %>%
  filter(forest_type == "managed") %>%
  rename(jurisdiction_code = jurisdiction) %>%
  select(jurisdiction_code, managed_area_prov = area_prov)

ALS_coverage_managed <-
  ALS_coverage_by_year_class2 %>%
  select(
    jurisdiction_code,
    year,
    managed_ALS_area_coverage,
    managed_ALS_area_total
  ) %>%
  left_join(managed_area_byProvince) %>%
  arrange(jurisdiction_code, year) %>%

  mutate(
    managed_ALS_area_coverage_prop = managed_ALS_area_coverage /
      managed_area_prov *
      100,
    managed_ALS_area_total_prop = managed_ALS_area_total /
      managed_area_prov *
      100,
  ) %>%
  # coverage cannot be >100%
  mutate(
    managed_ALS_area_coverage_prop = if_else(
      managed_ALS_area_coverage_prop > 100,
      100,
      managed_ALS_area_coverage_prop
    )
  )


# combine everything into one df ####
ALS_coverage <-
  ALS_coverage %>%
  left_join(ALS_coverage_managed)

ALS_coverage2 <-
  ALS_coverage %>%
  select(-total_area, -managed_area_prov) %>%
  rename(
    total_ALS_acquisition_area = total_acquisition_area,
    cumulative_total_ALS_acquisition_area = cumulative_total_acquisition_area,
    managed_ALS_coverage_area = managed_ALS_area_coverage,
    managed_ALS_acquisition_area = managed_ALS_area_total,
    managed_ALS_acquisition_area_perc = managed_ALS_area_total_prop,
    managed_ALS_area_coverage_perc = managed_ALS_area_coverage_prop,
  ) %>%
  #convert to thousands km2
  mutate(across(ends_with("area"), ~ as.numeric(.) / 1000))

# change NAs to 0 and recalculate cumulative area
ALS_coverage2 <-
  ALS_coverage2 %>%
  mutate(
    total_ALS_acquisition_area = replace_na(
      total_ALS_acquisition_area,
      replace = 0
    )
  ) %>%
  group_by(jurisdiction_code) %>%
  mutate(
    cumulative_total_ALS_acquisition_area = cumsum(total_ALS_acquisition_area)
  )

# saveRDS(ALS_coverage, "layers/coverageOverTime.rds")

A <-
  ALS_coverage2 %>%
  mutate(
    tip = glue::glue(
      "{jurisdiction_code}\n{year}\nP<sub>J</sub>={round(total_ALS_coverage_area_perc,1)}%\nP<sub>M</sub>={round(managed_ALS_area_coverage_perc,1)}%"
    )
  )
saveRDS(A, data_for_figure_v4_file)

# ALS_coverage2 %>% glimpse()
# ALS_coverage2 %>% View()

# # test plots
# ALS_coverage2 %>%
#   ggplot(aes(year, total_ALS_acquisition_area, color = jurisdiction_code)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = clrs, name = "")
# ALS_coverage2 %>%
#   ggplot(aes(year, total_ALS_coverage_area_perc, color = jurisdiction_code)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = clrs, name = "")
# ALS_coverage2 %>%
#   ggplot(aes(year, total_ALS_coverage_area, color = jurisdiction_code)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = clrs, name = "")
# ALS_coverage2 %>%
#   ggplot(aes(
#     year,
#     cumulative_total_ALS_acquisition_area,
#     color = jurisdiction_code
#   )) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = clrs, name = "")
# ALS_coverage2 %>%
#   ggplot(aes(year, managed_ALS_coverage_area, color = jurisdiction_code)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = clrs, name = "")
# ALS_coverage2 %>%
#   ggplot(aes(year, managed_ALS_acquisition_area, color = jurisdiction_code)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = clrs, name = "")
# ALS_coverage2 %>%
#   ggplot(aes(year, managed_ALS_area_coverage_prop, color = jurisdiction_code)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = clrs, name = "")

# #
