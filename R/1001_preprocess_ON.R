source("R/0000_setup.R")

# preprocess ON data

acquisition_file <- "layers/source_layers/ON/OMNR_Acquisition_Y1_to_Y8.shp"
tile_index_file <- "layers/source_layers/ON/FRI_Leaf_On_Tile_Index_GeoPackage.gpkg"
density_file <- "layers/source_layers/ON/ALS_ON_Y1_to_Y8_wDensity.gpkg"
density_cache_dir <- "layers/pre-processed/ON/density_samples"
laz_temp_dir <- "layers/pre-processed/ON/laz_temp"
n_tiles_per_acquisition <- 5
on_output_paths <- coverage_output_paths("ON")

dir_create(on_output_paths$dir)
dir_create(density_cache_dir)
dir_create(laz_temp_dir)

density_file_is_ready <- function(x) {
  if (!file.exists(x)) {
    return(FALSE)
  }

  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    return(FALSE)
  }

  con <- try(
    DBI::dbConnect(
      RSQLite::SQLite(),
      x,
      flags = RSQLite::SQLITE_RO,
      synchronous = NULL
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    return(FALSE)
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  layers <- try(
    DBI::dbGetQuery(con, "SELECT table_name FROM gpkg_contents"),
    silent = TRUE
  )

  if (inherits(layers, "try-error")) {
    return(FALSE)
  }

  "ALS_ON_Y1_to_Y8_wDensity" %in% layers$table_name
}

standardize_geometry_column <- function(x, name = "geometry") {
  geom_col <- attr(x, "sf_column")

  if (!identical(geom_col, name)) {
    names(x)[names(x) == geom_col] <- name
    st_geometry(x) <- name
  }

  x
}

read_las_density <- function(laz_file) {
  header <- lidR::readLASheader(laz_file)
  phb <- header@PHB

  clean_name <- function(x) {
    str_to_lower(str_remove_all(x, "[^A-Za-z0-9]"))
  }

  header_value <- function(candidates, positive = FALSE) {
    nm <- clean_name(names(phb))
    candidates <- clean_name(candidates)
    idx <- match(candidates, nm, nomatch = 0)
    idx <- idx[idx > 0]

    for (i in idx) {
      value <- as.numeric(phb[[i]][1])
      if (!is.na(value) && (!positive || value > 0)) {
        return(value)
      }
    }

    NA_real_
  }

  n_pts <- header_value(c(
    "Extended number of point records",
    "Number of point records",
    "Number of point records legacy"
  ), positive = TRUE)

  n_by_return_idx <- which(clean_name(names(phb)) %in% clean_name(c(
    "Extended number of points by return",
    "Number of points by return"
  )))
  n_first <- NA_real_
  if (length(n_by_return_idx) > 0) {
    n_first <- map_dbl(phb[n_by_return_idx], ~ as.numeric(.x[1])) %>%
      keep(~ !is.na(.x) && .x > 0) %>%
      first(default = NA_real_)
  }

  min_x <- header_value("Min X")
  max_x <- header_value("Max X")
  min_y <- header_value("Min Y")
  max_y <- header_value("Max Y")
  area_m2 <- (max_x - min_x) * (max_y - min_y)

  if (is.na(n_pts) || is.na(area_m2) || area_m2 <= 0) {
    als <- lidR::readLAS(laz_file)
    info <- capture.output(print(als))

    return(tibble(
      densityPts = readr::parse_number(info[7]),
      densityPls = readr::parse_number(info[8])
    ))
  }

  tibble(
    densityPts = n_pts / area_m2,
    densityPls = n_first / area_m2
  )
}

get_density <- function(url) {
  tilename <- str_remove(fname(url), "\\.copc$")
  fout <- file.path(density_cache_dir, glue("{tilename}.rds"))

  if (!file.exists(fout)) {
    message(glue("Downloading density sample: {tilename}"))

    temp_file <- file.path(laz_temp_dir, glue("{tilename}.laz"))
    if (file.exists(temp_file)) {
      file.remove(temp_file)
    }

    density <- tryCatch(
      {
        download.file(
          url = url,
          destfile = temp_file,
          method = "wininet",
          mode = "wb"
        )

        read_las_density(temp_file)
      },
      error = function(e) {
        warning(glue("Density sample failed for {tilename}: {conditionMessage(e)}"))

        tibble(
          densityPts = NA_real_,
          densityPls = NA_real_
        )
      }
    )

    density <- density %>% mutate(Tilename = tilename, .before = 1)
    saveRDS(density, fout)

    if (file.exists(temp_file)) {
      file.remove(temp_file)
    }
  }

  readRDS(fout)
}

read_acquisitions <- function() {
  st_read(acquisition_file, quiet = TRUE) %>%
    st_zm() %>%
    st_make_valid() %>%
    mutate(
      YEAR = readr::parse_number(str_extract(Block, "\\d{4}")),
      acquisition_id = as.character(glue("{YEAR}_{row_number()}"))
    ) %>%
    select(acquisition_id, YEAR, Block, geometry)
}

if (density_file_is_ready(density_file)) {
  ALS_ON_wDensity <- st_read(density_file, quiet = TRUE)
} else {
  density_file_parts <- Sys.glob(glue("{density_file}*"))
  if (length(density_file_parts) > 0) {
    invisible(file.remove(density_file_parts))
  }

  ALS_ON <- read_acquisitions()

  ON_tiles <- st_read(tile_index_file, layer = "FRI_Tile_Index", quiet = TRUE) %>%
    select(Tilename, Download_LAZ)

  ALS_ON_tiles_crs <- st_transform(ALS_ON, crs = st_crs(ON_tiles))

  ON_tile_points <- ON_tiles %>%
    st_point_on_surface()

  set.seed(123)
  ON_tiles_sample <- st_join(
    ON_tile_points,
    ALS_ON_tiles_crs %>% select(acquisition_id, YEAR),
    join = st_within,
    left = FALSE
  ) %>%
    st_drop_geometry() %>%
    filter(!is.na(Download_LAZ), Download_LAZ != "") %>%
    group_by(acquisition_id) %>%
    slice_sample(n = n_tiles_per_acquisition) %>%
    ungroup()

  density_results <- ON_tiles_sample$Download_LAZ %>%
    map(get_density) %>%
    bind_rows()

  density_summary <- ON_tiles_sample %>%
    left_join(density_results, by = "Tilename") %>%
    group_by(acquisition_id, YEAR) %>%
    summarise(
      n_tiles = n(),
      n_density = sum(!is.na(densityPts)),
      densityPts_mean = mean(densityPts, na.rm = TRUE),
      densityPts_sd = sd(densityPts, na.rm = TRUE),
      densityPls_mean = mean(densityPls, na.rm = TRUE),
      densityPls_sd = sd(densityPls, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      across(starts_with("density"), ~ if_else(is.nan(.x), NA_real_, .x))
    )

  ALS_ON_wDensity <- ALS_ON %>%
    left_join(density_summary, by = c("acquisition_id", "YEAR")) %>%
    mutate(
      densityPts_mean = if_else(is.na(densityPts_mean), 30, densityPts_mean),
      densityPls_mean = if_else(is.na(densityPls_mean), 30, densityPls_mean)
    )

  st_write(
    ALS_ON_wDensity,
    dsn = density_file,
    layer = "ALS_ON_Y1_to_Y8_wDensity",
    append = FALSE
  )
}

ALS_ON_wDensity <- standardize_geometry_column(ALS_ON_wDensity)

ALS_ON <- ALS_ON_wDensity %>%
  st_zm() %>%
  st_transform(crs = the_crs) %>%
  st_make_valid() %>%
  mutate(
    YEAR = as.integer(YEAR),
    PPM = round(densityPls_mean)
  ) %>%
  group_by(YEAR, PPM) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_as_sf() %>%
  mutate(
    area = st_area(geometry),
    Province = "ON",
    isAvailable = 2
  ) %>%
  relocate(Province, YEAR, PPM, area, isAvailable)

st_write(ALS_ON, dsn = on_output_paths$file, append = FALSE)

# without overlaps - newest acquisition kept
ALS_ON_diss <- remove_overlaps_by_attr(ALS_ON, "YEAR")

# update area
ALS_ON_diss <- ALS_ON_diss %>% mutate(area = st_area(geometry))

st_write(
  ALS_ON_diss,
  dsn = on_output_paths$diss_file,
  append = FALSE
)
