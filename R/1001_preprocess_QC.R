# Preprocessing acquisition data for Quebec

# data can be downloaded from https://www.donneesquebec.ca/recherche/dataset/produits-derives-de-base-du-lidar
# and no additional complicated processing is required.

# new files are put to layers/source_layers/QC

# newest file is selected when running the script

#directory with coverage files
thePath <- "layers/source_layers/QC"

# list all files, with modification time
flist <- dir_info(thePath, recurse = FALSE, glob="*.shp")

# get the newest - this file will be used in almost all processing
f <- flist %>% arrange(desc(modification_time)) %>%  slice(1) %>% pull(path)

# ALS coverage
ALS_QC <- st_read(f)

# ALS_QC <- st_read("layers/QC/Index_Acquisition_LiDAR_Mars_2022.shp") #old
# ALS_QC <- st_read("layers/source_layers/QC/Index_Acquisition_Juillet_2023.shp") #new 2014-04-09

ALS_QC %<>% mutate(PPM=parse_number(DENS_PT))

ALS_QC_diss <-
  ALS_QC %>% 
  st_zm() %>%
  select(YEAR=AN_ACQ, PPM) %>%
  st_transform(crs = the_crs) %>%
  st_make_valid() %>% 
  mutate(area = st_area(.)) %>% 
  group_by(YEAR, PPM) %>%
  summarise(n=n(), area=sum(area), geometry = st_union(geometry)) %>%
  mutate(Province="QC") %>%
  mutate(isAvailable=3) %>%
  st_cast() %>%
  select(Province, YEAR, PPM, n, area, isAvailable) 
