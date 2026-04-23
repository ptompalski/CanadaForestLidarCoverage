### downloading random tiles to get info on point density

ON_tiles <- st_read("layers/ON/FRI_Leaf_On_Tile_Index_GeoPackage.gpkg")

#create an alternative version of acquisition polygons, without combining polygons by year (keep original polys)
#this will help to pick random tiles to download - they will be downloaded by acquisition poly

#original polys for 2018-2021
ALS_ON <- st_read("layers/ON/OMNR_Y1_to_Y4_Blocks.shp")
ALS_ON <- ALS_ON %>% st_zm() %>% st_transform(crs = the_crs)
ALS_ON <- ALS_ON %>%
  mutate(Year = as.numeric(Year), FID = paste0(Year, "_", row_number())) %>%
  select(FID, Year, geometry)

#multipart to singlepart for 2022 and 2023
ALS_ON_2022 <- st_read("layers/ON/2022_combined.shp")
ALS_ON_2023 <- st_read("layers/ON/2023_combined.shp")

ALS_ON_2022 <- st_cast(ALS_ON_2022, "POLYGON") %>%
  mutate(Year = 2022, FID = paste0(Year, "_", row_number())) %>%
  select(FID, Year, geometry)
ALS_ON_2023 <- st_cast(ALS_ON_2023, "POLYGON") %>%
  mutate(Year = 2023, FID = paste0(Year, "_", row_number())) %>%
  select(FID, Year, geometry)


ALS_ON <- bind_rows(ALS_ON, ALS_ON_2022)
ALS_ON <- bind_rows(ALS_ON, ALS_ON_2023)

# plot(ALS_ON["Year"])

#intersect with acquisition year layer
ALS_ON2 <- st_transform(ALS_ON, crs = st_crs(ON_tiles))

# st_crs(ON_tiles)== st_crs(ALS_ON2)

# a <- st_is_valid(ON_tiles) #all are ok
# st_is_valid(ALS_ON2)

ON_tiles_inters <- st_intersection(ON_tiles, ALS_ON2)

#pick 5 sites per poly
set.seed(123)
ON_tiles_sample <-
  ON_tiles_inters %>%
  as_tibble() %>%
  group_by(FID) %>% #count() %>% print(n=100)
  slice_sample(n = 5)


getDensity <- function(url) {
  tilename <- str_remove(fname(url), ".copc")

  fout <- glue("data/ON/{tilename}.rds")

  if (!file.exists(fout)) {
    print(fout)

    temp_file <- glue("data/als_wb_{tilename}.laz")

    if (file.exists(temp_file)) {
      file.remove(temp_file)
    }

    download.file(
      url = url,
      destfile = temp_file,
      method = "wininet",
      mode = "wb"
    )

    #get als file info
    als <- lidR::readLAS(temp_file)
    info <- capture.output(print(als))

    #save
    info2 <- tibble(
      Tilename = tilename,
      densityPts = readr::parse_number(info[7]),
      densityPls = readr::parse_number(info[8])
    )

    saveRDS(info2, fout)

    file.remove(temp_file)
  }
}

URLs <- ON_tiles_sample$Download_LAZ

lapply(URLs, getDensity)

# library(future.apply)
# plan(multisession(workers = 4))
# options(future.globals.onReference = "error") #helps to identify errors
# options(future.globals.maxSize= 1100289600)
# future_lapply(URLs, getDensity)
# plan(sequential)

#post-process results

R <- list.files("data/ON/", full.names = T) %>%
  map(readRDS) %>%
  bind_rows()

R <- ON_tiles_sample %>%
  left_join(R) %>%
  select(Tilename, FID, Year, starts_with("density"))

R <- R %>%
  group_by(FID) %>% #count() %>% arrange(n)
  summarise(across(starts_with("density"), list(mean = mean, sd = sd)))

#link to acquisitions

ALS_ON_wDensity <- ALS_ON %>% left_join(R)

plot(ALS_ON_wDensity["densityPts_mean"])

#any polys withouth density info?
ALS_ON_wDensity %>% filter(is.na(densityPts_mean))
#those are not availalble to download

#if density missing setting it to 30 pts/m2
ALS_ON_wDensity <-
  ALS_ON_wDensity %>%
  mutate(
    densityPts_mean = if_else(is.na(densityPts_mean), 30, densityPts_mean),
    densityPls_mean = if_else(is.na(densityPls_mean), 30, densityPls_mean)
  )

#save
st_write(ALS_ON_wDensity, dsn = "layers/ON/ALS_ON_wDensity_2018-2023.shp")
