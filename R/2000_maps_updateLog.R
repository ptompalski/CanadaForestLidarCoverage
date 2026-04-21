# show which areas where updated / added after the recent update

# source("R/2000_maps_setup.R")

#get two newest coverage files
f_two_most_recent <- latest_files_by_pattern(
  file.path(PATH, "main/ALS_coverage_all_*.rds"),
  n = 2,
  stamp_regex = "ALS_coverage_all_(\\d{8})\\.rds",
  label = "main ALS coverage RDS"
)

#get the date of the newest
updateDate <- str_match(
  basename(f_two_most_recent[1]),
  "ALS_coverage_all_(\\d{8})\\.rds"
)[, 2] %>%
  ymd() %>%
  as.character()

#read
coverage1 <- readRDS(f_two_most_recent[1]) #newest
coverage2 <- readRDS(f_two_most_recent[2]) #previous



# coverage1_snapped <- st_snap(coverage1, coverage2, tolerance = 0.5)

# difference
coverage_change <- st_difference(coverage1, st_union(coverage2))

#filter slivers
coverage_change <- coverage_change %>%  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
coverage_change <- coverage_change %>% mutate(area = as.numeric(st_area(.)))
coverage_change <- coverage_change %>% filter(area > 1)

coverage_change$Name <- "New ALS coverage"
D_dissolved$Name <- "Existing ALS coverage"

## MAPS ###


existing_col <- "grey" #"#157A6E" #"#41ab5d"
new_col <- "red"

map_newAcquisitions <- 
  ggplot(data=NULL) + WATER + 
  ECOZONES + 
  ECOZONES_formatting+
  new_scale_fill()+
  
  geom_sf(data=D_dissolved, aes(fill=factor(Name)), color=existing_col)+
  scale_fill_manual(values=existing_col, name=NULL)+
  
  new_scale_fill()+
  geom_sf(data=coverage_change, aes(fill=factor(Name)), color=new_col)+
  scale_fill_manual(values=new_col, name=NULL)+
  
  JURISDICTIONS+LABELS+
  
  EXTENT + THEME_SETTINGS + SCALEBAR + CREDITS #+LOGO 


fout <- glue("img/UpdateLog/map_newAcquisitions_{updateDate}.png")
save_map_with_logo(map_newAcquisitions, fout, width = 7, height = 5, dpi = 300)
# 
# #add logo using magick
# background <- image_read(fout)
# logo_resized <- magick::image_resize(logo, geometry = "500x")
# newImg <- image_composite(background, logo_resized, gravity = "SouthWest", offset = "+30+50")
# image_write(newImg, fout)
