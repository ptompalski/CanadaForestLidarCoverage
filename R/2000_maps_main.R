# source("R/2000_maps_setup.R")

### MAPS MAIN ####

#### map0 - overview ####
#will be on the main www

overview_col <- "#157A6E" #"#157A6E" #"#41ab5d"


map_overview <-
  ggplot(data = NULL) +
  WATER +
  ECOZONES +
  ECOZONES_formatting +
  new_scale_fill() +

  geom_sf(data = D_dissolved, aes(fill = factor(Name)), color = overview_col) +
  scale_fill_manual(values = overview_col, name = NULL) +

  JURISDICTIONS +
  LABELS +

  EXTENT +
  THEME_SETTINGS +
  SCALEBAR +
  CREDITS #+LOGO

# map_overview

# map_density
fout <- "img/map0_overview.png"
ggsave(plot = map_overview, filename = fout, width = 7, height = 5, dpi = 300)

# add logo using magick
background <- image_read(fout)
logo_resized <- magick::image_resize(logo, geometry = "500x")
newImg <- image_composite(
  background,
  logo_resized,
  gravity = "SouthWest",
  offset = "+30+50"
)
image_write(newImg, fout)


#### map1 - overall coverage ####

# r <- rast("layers/manage_unmanaged_v2_aggreg.tif")
# D1 <- st_read("C:/Users/ptompals/OneDrive - NRCan RNCan/__WORK/5_lidarStatusCanada/ALS_coverage_all_2024_generalized_v2.gpkg")
# D1$name <- "ALS data coverage"
# r <- terra::as.factor(r)

map_coverage <-
  ggplot() +
  WATER +
  ECOZONES3 +
  geom_spatraster(data = r, aes(fill = managed_forest_mask)) +
  scale_fill_coltab(
    data = r,
    name = "Forest type",
    labels = c("Managed", "Unmanaged")
  ) +

  JURISDICTIONS +

  geom_sf_pattern(
    data = D1,
    aes(pattern = name),
    color = "black",
    linewidth = 0.15,
    fill = NA,
    pattern_fill = "black",
    pattern_fill2 = NA,
    pattern_frequency = 1,
    pattern_size = 0.005,
    pattern_density = 0.05,
    pattern_spacing = 0.005
  ) +

  scale_pattern_discrete(name = NULL) +
  LABELS +
  EXTENT +
  THEME_SETTINGS +
  SCALEBAR +
  CREDITS +
  theme(legend.title = element_text(size = 10)) # +LOGO


fout <- "img/map1_ALS_coverage.png"
ggsave(plot = map_coverage, filename = fout, width = 7, height = 5)

#add logo using magick
background <- image_read(fout)
# And bring in a logo
# logo_raw <- image_read("https://i.imgur.com/e1IneGq.jpg")
logo
logo_resized <- magick::image_resize(logo, geometry = "500x")

newImg <- image_composite(
  background,
  logo_resized,
  gravity = "SouthWest",
  offset = "+30+50"
)
newImg
image_write(newImg, fout)


#### map2 - density ####

map_density <-
  ggplot(data = NULL) +
  WATER +
  ECOZONES +
  ECOZONES_formatting +
  new_scale_fill() +

  geom_sf(data = Dx, aes(fill = PPM_class), color = NA) +

  JURISDICTIONS +
  LABELS +

  scale_fill_manual(
    values = as.character(MetBrewer::met.brewer(name = "Hokusai2", n = 7)),
    name = "ALS data density",
    na.translate = FALSE,
    guide = guide_legend(
      ncol = 2,
      order = 1,
      title.position = "top",
      reverse = F
    )
  ) +
  EXTENT +
  THEME_SETTINGS +
  SCALEBAR +
  CREDITS #+LOGO

# map_density
fout <- "img/map2_ALS_density.png"
ggsave(plot = map_density, filename = fout, width = 7, height = 5, dpi = 300)

#add logo using magick
background <- image_read(fout)
logo_resized <- magick::image_resize(logo, geometry = "500x")
newImg <- image_composite(
  background,
  logo_resized,
  gravity = "SouthWest",
  offset = "+30+50"
)
image_write(newImg, fout)


#### map3 - acquisition year ####

map_acquisitionYear <-
  ggplot(data = NULL) +
  WATER +
  ECOZONES +
  ECOZONES_formatting +
  new_scale_fill() +

  geom_sf(data = Dx, aes(fill = (YEAR)), color = NA) +

  JURISDICTIONS +
  LABELS +

  scale_fill_gradientn(
    name = "Acquisition year",
    colours = rev(as.character(MetBrewer::met.brewer(
      name = "Demuth",
      n = length(unique(D$YEAR))
    ))),
    breaks = acquisition_year_breaks,
    guide = guide_colorbar(
      order = 1,
      theme = theme(
        legend.direction = "horizontal",
        legend.key.width = unit(1.5, "inches")
      ),
      ticks.colour = "black",
      # draw.ulim = FALSE,
      # draw.llim = FALSE,
      title.position = "top"
    )
  ) +

  EXTENT +
  THEME_SETTINGS +
  theme(
    legend.box.just = "right",
    legend.title = element_text(hjust = 0.5),
    legend.spacing.y = unit(0, "cm")
  ) +

  SCALEBAR +
  CREDITS #+LOGO

fout <- "img/map3_ALS_AcquisitionYear.png"
ggsave(
  plot = map_acquisitionYear,
  filename = fout,
  width = 7,
  height = 5,
  dpi = 300
)

#add logo using magick
background <- image_read(fout)
logo_resized <- magick::image_resize(logo, geometry = "500x")
newImg <- image_composite(
  background,
  logo_resized,
  gravity = "SouthWest",
  offset = "+30+50"
)
image_write(newImg, fout)


#### map4 - overlap areas ####

# O <- st_read("C:/Users/ptompals/OneDrive - NRCan RNCan/__WORK/5_lidarStatusCanada/ALS_coverage_multitemporal_v2.gpkg")
#
# #exclude delta year of a year or less
# O %<>% filter(YEAR_delta > 1)
#
# O$delta_class <- cut(O$YEAR_delta, breaks=c(1,2,5,10,15,20), labels=c("1-2", "2-5", "5-10", "10-15",">15"))

map_overlap <-
  ggplot(data = NULL) +
  WATER +
  ECOZONES +
  ECOZONES_formatting +
  new_scale_fill() +

  geom_sf(data = O, aes(fill = (delta_class)), color = NA) +

  JURISDICTIONS +
  LABELS +

  scale_fill_manual(
    values = viridis::magma(6)[2:6],
    # values = viridis::magma(5),
    name = "Years between overlapping acquisitions",
    guide = guide_legend(
      ncol = 5,
      order = 1,
      title.position = "top",
      reverse = F
    )
  ) +

  EXTENT +
  THEME_SETTINGS +
  theme(
    legend.box.just = "right",
    legend.title = element_text(hjust = 0.5),
    legend.spacing.y = unit(0.2, "cm")
  ) +

  SCALEBAR +
  CREDITS #+LOGO

# ggsave(plot = map_overlap, filename = "img/map4_ALS_overlap.png", width = 7, height=5)
fout <- "img/map4_ALS_overlap.png"
ggsave(plot = map_overlap, filename = fout, width = 7, height = 5, dpi = 300)

#add logo using magick
background <- image_read(fout)
logo_resized <- magick::image_resize(logo, geometry = "500x")
newImg <- image_composite(
  background,
  logo_resized,
  gravity = "SouthWest",
  offset = "+30+50"
)
image_write(newImg, fout)
