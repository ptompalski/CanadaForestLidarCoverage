if (!exists("the_crs") || !exists("coverage_output_paths")) {
  source("R/0000_setup.R")
}

gc_source_file <- Sys.getenv(
  "GC_METADATA_GDB_FILE",
  unset = "layers/source_layers/GC/Metadata_PointCloud_NRCAN.gdb"
)
gc_source_layer <- "metadata_2"
gc_output_paths <- coverage_output_paths("GC")

existing_preprocess_targets <- c("AB", "BC", "NB", "NS", "ON", "PEI", "QC", "SK")
existing_jurisdiction_codes <- c("AB", "BC", "NB", "NS", "ON", "PE", "QC", "SK")

province_boundaries <- read_province_boundaries() %>%
  st_transform(crs = the_crs) %>%
  select(Province = PROV)
supported_provinces <- unique(provinces_area$jurisdiction_code)

standardize_coverage_layer <- function(x) {
  geom_col <- attr(x, "sf_column")

  if (!identical(geom_col, "geometry")) {
    names(x)[names(x) == geom_col] <- "geometry"
    st_geometry(x) <- "geometry"
  }

  x %>%
    mutate(
      YEAR = readr::parse_integer(as.character(YEAR)),
      PPM = as.numeric(PPM),
      area = units::set_units(as.numeric(area), m^2)
    ) %>%
    select(Province, YEAR, PPM, area, isAvailable, geometry)
}

ALS_GC <- st_read(gc_source_file, layer = gc_source_layer, quiet = TRUE) %>%
  st_transform(crs = the_crs) %>%
  st_make_valid() %>%
  {
    st_geometry(.) <- "SHAPE"
    .
  } %>%
  {
    names(.)[names(.) == "SHAPE"] <- "geometry"
    st_geometry(.) <- "geometry"
    .
  } %>%
  mutate(
    YEAR = readr::parse_integer(substr(as.character(TEMPORAL_EXTENT_DATE_MAX), 1, 4)),
    PPM = as.numeric(LDR_AGGREGATE_DENSITY)
  ) %>%
  select(YEAR, PPM) %>%
  filter(!is.na(YEAR), !is.na(PPM), !st_is_empty(geometry))

gc_supplement <- ALS_GC %>%
  filter(!st_is_empty(geometry)) %>%
  clean_coverage_polygons() %>%
  st_intersection(province_boundaries) %>%
  filter(Province %in% supported_provinces) %>%
  filter(!st_is_empty(geometry)) %>%
  group_by(Province, YEAR, PPM) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(
    area = units::set_units(as.numeric(st_area(geometry)), m^2),
    isAvailable = 1
  ) %>%
  select(Province, YEAR, PPM, area, isAvailable)

st_write(gc_supplement, dsn = gc_output_paths$file, append = FALSE)

existing_dissolved <- read_preprocessed_coverage(
  existing_preprocess_targets,
  dissolved = TRUE
) %>%
  map(standardize_coverage_layer) %>%
  bind_rows()

gc_existing_overlap <- gc_supplement %>%
  filter(Province %in% existing_jurisdiction_codes)
gc_no_existing_source <- gc_supplement %>%
  filter(!Province %in% existing_jurisdiction_codes)

gc_reconciled <- bind_rows(
  gc_no_existing_source,
  bind_rows(
    existing_dissolved %>%
      filter(Province %in% existing_jurisdiction_codes) %>%
      mutate(source_rank = 0L, source_tag = "existing"),
    gc_existing_overlap %>%
      mutate(source_rank = 1L, source_tag = "gc")
  ) %>%
    group_split(Province) %>%
    map_dfr(
      ~ .x %>%
        arrange(desc(YEAR), source_rank) %>%
        remove_overlaps_by_attr("YEAR") %>%
        filter(source_tag == "gc") %>%
        select(-source_rank, -source_tag)
    )
)

ALS_GC_diss <- gc_reconciled
ALS_GC_diss <- ALS_GC_diss %>%
  mutate(area = units::set_units(as.numeric(st_area(geometry)), m^2))

st_write(
  ALS_GC_diss,
  dsn = gc_output_paths$diss_file,
  append = FALSE
)
