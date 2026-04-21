# source("R/2000_maps_setup.R")

# # Sys.setenv(PROJ_NETWORK = "OFF")
# # proj_dir <- file.path(Sys.getenv("APPDATA"), "proj")
# # dir.create(proj_dir, showWarnings = FALSE, recursive = TRUE)
# Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT = TRUE)
# Sys.setenv(CURLSSLOPT_REVOKE_BEST_EFFORT = TRUE = TRUE)
# Sys.setenv(PROJ_USER_WRITABLE_COPY = "YES")
# Sys.setenv(PROJ_DATA = file.path(Sys.getenv("APPDATA"), "proj"))

### FOCUS MAPS (WEST AND EAST) ####

#### setup ####
width_px_f1 <- 1960
height_px_f1 <- 1500
width_px_f2 <- 1900
height_px_f2 <- 1400

# +proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs
crs_focus1 <- "+proj=lcc +lat_0=63.390675 +lon_0=-115 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
crs_focus2 <- "+proj=lcc +lat_0=63.390675 +lon_0=-80 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"


# focus1 <- provinces2 %>% filter(PROV %in% c("BC","AB")) %>%
focus1 <- D %>%
  filter(Province %in% c("BC", "AB", "SK")) %>%
  st_transform(crs = crs_focus1) %>%
  st_bbox()

focus2 <- D %>%
  filter(Province %in% c("ON", "QC", "NS")) %>%
  st_transform(crs = crs_focus2) %>%
  st_bbox()
# add some buffer to the north
# focus2[4] <- focus2[4] + 400000

EXTENT_F1 <- coord_sf(
  crs = crs_focus1,
  xlim = c(focus1[1], focus1[3]),
  ylim = c(focus1[2], focus1[4]),
  expand = T,
  clip = "on"
)

EXTENT_F2 <- coord_sf(
  crs = crs_focus2,
  xlim = c(focus2[1], focus2[3]),
  ylim = c(focus2[2], focus2[4]),
  expand = T
)

CREDITS2 <- labs(caption = credits_text)

ECOZONES2 <- geom_sf(
  data = ecozones_forested,
  fill = "#c2e699",
  color = "#d8efbc",
  alpha = 0.25
)

THEME_SETTINGS2 <-
  ggthemes::theme_map(base_family = thefont) +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(color = "grey40"),
    # legend.margin  = margin(t = -0.5, unit='cm'),
    legend.box.margin = margin(t = 0, b = 0.2, unit = 'cm')
  )


# #LOGO for west
# # calculating position and size of the logo - focus maps west
# size_x <- as.numeric(focus1[3]-focus1[1])
# logo_prop <- 0.20
#
# logo_size_x <- size_x * logo_prop
# logo_size_y <- logo_size_x * 11.4/49
#
# logo_xmin <- focus1[1] - 80000 #-2289876
# logo_xmax <- logo_xmin + logo_size_x
#
# logo_ymin <- focus1[2] - 20000 #-704116
# logo_ymax <- logo_ymin + logo_size_y
#
# LOGO_F1 <- annotation_raster(raster = logo, xmin = logo_xmin, xmax = logo_xmax, ymin = logo_ymin, ymax = logo_ymax)
#
#
# #LOGO for east
# # calculating position and size of the logo - focus maps west
# size_x <- as.numeric(focus2[3]-focus2[1])
# logo_prop <- 0.20
#
# logo_size_x <- size_x * logo_prop
# logo_size_y <- logo_size_x * 11.4/49
#
# logo_xmin <- focus2[1] - 80000 #-2289876
# logo_xmax <- logo_xmin + logo_size_x
#
# logo_ymin <- focus2[2] - 20000 #-704116
# logo_ymax <- logo_ymin + logo_size_y
#
# LOGO_F2 <- annotation_raster(raster = logo, xmin = logo_xmin, xmax = logo_xmax, ymin = logo_ymin, ymax = logo_ymax)

SCALEBAR_F1 <- ggspatial::annotation_scale(
  width_hint = 0.1,
  style = "ticks",
  # pad_x = unit(0, "cm"),
  # pad_y = unit(-1, "cm"),
  text_family = thefont,
  location = "br",
  tick_height = 0
)


SCALEBAR_F2 <- ggspatial::annotation_scale(
  width_hint = 0.1,
  style = "ticks",
  pad_x = unit(0.25, "in"),
  pad_y = unit(0.3, "in"),
  text_family = thefont,
  location = "tr",
  tick_height = 0
)


#### need to redo main maps with slightly differnt settings ####
# redo the plots with limited customization to then tinker with for each focus area separately

map_density_forFocusedMaps <-
  ggplot(data = NULL) +
  WATER +
  ECOZONES2 +
  geom_sf(data = Dx, aes(fill = PPM_class), color = NA) +
  JURISDICTIONS +
  LABELS +
  scale_fill_manual(
    values = as.character(MetBrewer::met.brewer(name = "Hokusai2", n = 7)),
    name = "ALS data density",
    na.translate = FALSE,
    guide = guide_legend(
      ncol = 7,
      order = 1,
      title.position = "top",
      reverse = F
    )
  ) +
  EXTENT +
  THEME_SETTINGS2


map_acquisitionYear_forFocusedMaps <-
  ggplot(data = NULL) +
  WATER +
  ECOZONES2 +
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
        legend.key.width = unit(2.5, "inches")
      ),
      ticks.colour = "black",
      # barwidth = 50,
      # draw.ulim = FALSE,
      # draw.llim = FALSE,
      title.position = "top"
    )
  ) +
  EXTENT +
  THEME_SETTINGS2


### overall coverage ###

p <- st_read("layers/manage_unmanaged_v2_aggreg.gpkg")

D1$name <- " "

map_coverage_forFocusedMaps <-
  ggplot() +
  ggthemes::theme_map(base_family = thefont) +
  WATER +

  geom_sf(data = p, aes(fill = (managed_forest_mask)), color = NA) +
  scale_fill_manual(
    name = "Forest type:",
    values = c("#41AB5D", "#A1D99B"),
    labels = c("Managed", "Unmanaged"),
    guide = guide_legend(
      order = 2,
      nrow = 1,
      title.position = "left",
      title.hjust = 0,
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      legend.key.size = unit(1, "lines")
    )
  ) +

  # geom_spatraster(data = r, aes(fill = managed_forest_mask), maxcell = 5e+08) +
  # scale_fill_coltab(
  #   data = r,
  #   name = "Forest type:",
  #   labels = c("Managed", "Unmanaged"),
  #   guide = guide_legend(
  #     order = 2,
  #     nrow = 1,
  #     title.position = "left",
  #     title.hjust = 0,
  #     legend.title = element_text(size = 7),
  #     legend.text = element_text(size = 7),
  #     legend.key.size = unit(1, "lines")
  #   )
  # ) +
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
  scale_pattern_discrete(
    name = "ALS data coverage",
    guide = guide_legend(
      order = 1,
      legend.text = element_blank(),
      legend.box.margin = margin(t = 4, unit = 'cm')
    )
  ) +
  LABELS +
  EXTENT +
  THEME_SETTINGS2

# ggsave(map_coverage_forFocusedMaps, filename = "coverage_test.png") # was ok

### overlap ###

map_overlap_forFocusedMaps <-
  ggplot(data = NULL) +
  WATER +
  ECOZONES2 +
  geom_sf(data = O, aes(fill = (delta_class)), color = NA) +
  JURISDICTIONS +
  LABELS +
  scale_fill_manual(
    values = viridis::magma(5),
    name = "Years between overlapping acquisitions",
    guide = guide_legend(
      ncol = 5,
      order = 1,
      # keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = F
    )
  ) +
  EXTENT +
  THEME_SETTINGS2


#### maps for focus areas ####

##### focus area 1 ####

map_density_f1 <-
  map_density_forFocusedMaps +
  EXTENT_F1 +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption = element_text(size = 5, color = "grey70", family = thefont),
    # legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
    legend.title = element_text(size = 11, hjust = 0),
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  CREDITS2 +
  SCALEBAR_F1 +
  #fill lower left corner with "water"
  annotate(
    geom = "rect",
    xmin = 5055364 - 999999,
    ymin = 1381188 - 999999,
    xmax = 5005364 + 196013,
    ymax = 1381188 + 500000,
    fill = "#eff3ff"
  )
# LOGO_F1

fout <- "img/map2_ALS_density_focused1.png"
save_map_with_logo(
  map_density_f1,
  fout,
  width = width_px_f1,
  height = height_px_f1,
  units = "px",
  logo_filter = "Hermite",
  logo_gravity = "SouthEast",
  logo_offset = "+40+85"
)


map_acquisitionYear_f1 <-
  map_acquisitionYear_forFocusedMaps +
  EXTENT_F1 +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption = element_text(size = 5, color = "grey70", family = thefont),
    # legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
    legend.title = element_text(size = 11, hjust = 0),
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  CREDITS2 +
  #fill lower left corner with "water"
  annotate(
    geom = "rect",
    xmin = 5055364 - 999999,
    ymin = 1381188 - 999999,
    xmax = 5005364 + 196013,
    ymax = 1381188 + 500000,
    fill = "#eff3ff"
  ) +
  SCALEBAR_F1
# ggsave(plot = map_acquisitionYear_f1, filename = "img/map3_ALS_AcquisitionYear_focused1.png", width = 1960, height=1500, units = "px")

fout <- "img/map3_ALS_AcquisitionYear_focused1.png"
save_map_with_logo(
  map_acquisitionYear_f1,
  fout,
  width = width_px_f1,
  height = height_px_f1,
  units = "px",
  logo_filter = "Hermite",
  logo_gravity = "SouthEast",
  logo_offset = "+40+85"
)


map_coverage_f1 <-
  map_coverage_forFocusedMaps +
  EXTENT_F1 +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption = element_text(size = 5, color = "grey70", family = thefont),
    # legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
    legend.title = element_text(size = 11, hjust = 0),
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  CREDITS2 +
  #fill lower left corner with "water"
  annotate(
    geom = "rect",
    xmin = 5055364 - 999999,
    ymin = 1381188 - 999999,
    xmax = 5005364 + 196013,
    ymax = 1381188 + 500000,
    fill = "#eff3ff"
  ) +
  SCALEBAR_F1

# ggsave(plot = map_coverage_f1, filename = "img/map1_ALS_coverage_focused1.png", width = 1960, height=1500, units = "px")
fout <- "img/map1_ALS_coverage_focused1.png"
save_map_with_logo(
  map_coverage_f1,
  fout,
  width = width_px_f1,
  height = height_px_f1,
  units = "px",
  logo_filter = "Hermite",
  logo_gravity = "SouthEast",
  logo_offset = "+40+85"
)


map_overlap_f1 <-
  map_overlap_forFocusedMaps +
  EXTENT_F1 +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption = element_text(size = 5, color = "grey70", family = thefont),
    # legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
    legend.title = element_text(size = 11, hjust = 0),
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  CREDITS2 +
  #fill lower left corner with "water"
  annotate(
    geom = "rect",
    xmin = 5055364 - 999999,
    ymin = 1381188 - 999999,
    xmax = 5005364 + 196013,
    ymax = 1381188 + 500000,
    fill = "#eff3ff"
  ) +
  SCALEBAR_F1
# ggsave(plot = map_overlap_f1, filename = "img/map4_ALS_overlap_focus1.png", width = 1960, height=1500, units = "px")

fout <- "img/map4_ALS_overlap_focus1.png"
save_map_with_logo(
  map_overlap_f1,
  fout,
  width = width_px_f1,
  height = height_px_f1,
  units = "px",
  logo_filter = "Hermite",
  logo_gravity = "SouthEast",
  logo_offset = "+40+85"
)


#####  focus area 2 #####

map_density_f2 <-
  map_density_forFocusedMaps +
  EXTENT_F2 +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption = element_text(size = 5, color = "grey70", family = thefont),
    # legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
    legend.title = element_text(size = 11, hjust = 0),
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  # theme(
  #   legend.position = "bottom",
  #   legend.justification = "center",plot.caption = element_text(size=5,color="grey70", family=thefont),
  #   legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
  # ) +
  CREDITS2 +
  SCALEBAR_F2

# ggsave(plot = map_density_f2, filename = "img/map2_ALS_density_focused2.png", width = 2100, height=1260, units = "px")

fout <- "img/map2_ALS_density_focused2.png"
save_map_with_logo(
  map_density_f2,
  fout,
  width = width_px_f2,
  height = height_px_f2,
  units = "px",
  logo_filter = "Hermite",
  logo_gravity = "SouthEast",
  logo_offset = "+40+85"
)
# ggsave(plot = map_density_f2, filename = fout, width = 2100, height=1260, units = "px")


map_acquisitionYear_f2 <-
  map_acquisitionYear_forFocusedMaps +
  EXTENT_F2 +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption = element_text(size = 5, color = "grey70", family = thefont),
    # legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
    legend.title = element_text(size = 11, hjust = 0),
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  CREDITS2 +
  SCALEBAR_F2 #+ LOGO_F2
# ggsave(plot = map_acquisitionYear_f2, filename = "img/map3_ALS_AcquisitionYear_focused2.png", width = 2100, height=1260, units = "px")

fout <- "img/map3_ALS_AcquisitionYear_focused2.png"
save_map_with_logo(
  map_acquisitionYear_f2,
  fout,
  width = width_px_f2,
  height = height_px_f2,
  units = "px",
  logo_filter = "Hermite",
  logo_gravity = "SouthEast",
  logo_offset = "+40+85"
)


map_coverage_f2 <-
  map_coverage_forFocusedMaps +
  EXTENT_F2 +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption = element_text(size = 5, color = "grey70", family = thefont),
    # legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
    legend.title = element_text(size = 11, hjust = 0),
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  CREDITS2 +
  SCALEBAR_F2
# ggsave(plot = map_coverage_f2, filename = "img/map1_ALS_coverage_focused2.png", width = 2100, height=1260, units = "px")

fout <- "img/map1_ALS_coverage_focused2.png"
save_map_with_logo(
  map_coverage_f2,
  fout,
  width = width_px_f2,
  height = height_px_f2,
  units = "px",
  logo_filter = "Hermite",
  logo_gravity = "SouthEast",
  logo_offset = "+40+85"
)


map_overlap_f2 <-
  map_overlap_forFocusedMaps +
  EXTENT_F2 +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption = element_text(size = 5, color = "grey70", family = thefont),
    # legend.title = element_text(size=11, hjust = 0.5), legend.margin = margin(t = 0, unit='cm')
    legend.title = element_text(size = 11, hjust = 0),
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  CREDITS2 +
  SCALEBAR_F2
# ggsave(plot = map_overlap_f2, filename = "img/map4_ALS_overlap_focus2.png", width = 2100, height=1260, units = "px")

fout <- "img/map4_ALS_overlap_focus2.png"
save_map_with_logo(
  map_overlap_f2,
  fout,
  width = width_px_f2,
  height = height_px_f2,
  units = "px",
  logo_filter = "Hermite",
  logo_gravity = "SouthEast",
  logo_offset = "+40+85"
)
