# prepare data for figure showing acquisition area over time
# this figure could show:
# a) total or cumulative area of acquisitions by year, including overlaps (overlaps counted)
# b) total or cumulative area of acquisitions by year, but excluding overlaps

# using data with acquisitions by year (not dissolved)
flist2 <- dir_info(
  file.path(PATH, "multitemporal"),
  glob = "*.gpkg$",
  recurse = FALSE
)
# get the newest - this file will be used in almost all processing
f2 <- flist2 %>% arrange(desc(modification_time)) %>% slice(1) %>% pull(path)
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

ALS_coverage_by_year_class <- purrr::map_dfr(years, function(y) {
  message("year: ", y)

  # 1) ---- DISSOLVED version ----
  Q_diss <- Q %>%
    dplyr::filter(YEAR <= y) %>%
    dplyr::group_by(Province) %>%
    dplyr::summarize(.groups = "drop")

  Q_diss_spat <- terra::vect(Q_diss)

  ex_diss <- terra::extract(
    manage_unmanaged_v2,
    Q_diss_spat,
    fun = table,
    na.rm = TRUE,
    bind = TRUE
  )

  ex_diss$Province <- Q_diss$Province

  dissolved_tbl <- ex_diss %>%
    tibble::as_tibble() %>%
    dplyr::rename(jurisdiction_code = Province) %>%
    dplyr::mutate(
      year = y,
      dissolved = TRUE
    ) %>%
    dplyr::select(jurisdiction_code, year, dissolved, `1`:`3`) %>%
    tidyr::pivot_longer(`1`:`3`, names_to = "class", values_to = "area_km2")

  # 2) ---- NON-DISSOLVED version ----
  Q_raw <- Q %>%
    dplyr::filter(YEAR <= y) %>%
    dplyr::mutate(poly_id = dplyr::row_number())

  Q_raw_spat <- terra::vect(Q_raw)

  ex_raw <- terra::extract(
    manage_unmanaged_v2,
    Q_raw_spat,
    fun = table,
    na.rm = TRUE,
    bind = TRUE
  )

  # make sure Province is there
  ex_raw$Province <- Q_raw$Province
  ex_raw$year <- y

  nondissolved_tbl <- ex_raw %>%
    tibble::as_tibble() %>%
    dplyr::rename(jurisdiction_code = Province) %>%
    # ensure poly_id exists even if terra dropped it
    {
      if (!"poly_id" %in% names(.)) {
        dplyr::mutate(., poly_id = dplyr::row_number())
      } else {
        .
      }
    } %>%
    # select safely (any_of won't error if col missing)
    dplyr::select(
      jurisdiction_code,
      year,
      dplyr::any_of("poly_id"),
      `1`:`3`
    ) %>%
    tidyr::pivot_longer(`1`:`3`, names_to = "class", values_to = "area_km2") %>%
    dplyr::group_by(jurisdiction_code, year, class) %>%
    dplyr::summarize(
      area_km2 = sum(area_km2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dissolved = FALSE)

  # 3) ---- combine ----
  dplyr::bind_rows(dissolved_tbl, nondissolved_tbl)
})


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
  file = "layers/coverageManagedUnmanaged.rds"
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
saveRDS(A, "layers/dataForTheFigure_v4.rds")

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
