source("R/0000_setup.R")
library(magick)
library(ggfx)
library(fs)

### setup ####

#directory with coverage files
PATH <- "layers/ALS_coverage_layer/"

f <- latest_file_by_pattern(
  file.path(PATH, "main/ALS_coverage_all_*.rds"),
  stamp_regex = "ALS_coverage_all_(\\d{8})\\.rds",
  label = "main ALS coverage RDS"
)

# ALS coverage
D <- readRDS(f)


#dissolve into one poly
D_dissolved <- D %>%
  ungroup() %>%
  st_union() %>%
  st_sf() %>%
  mutate(Name = "ALS data coverage")

#make sure year is a number
D$YEAR <- as.numeric(D$YEAR)
D$PPM <- as.numeric(D$PPM)

year_max <- max(D$YEAR, na.rm = TRUE)
acquisition_year_breaks <- seq(2005, floor(year_max / 5) * 5, by = 5)

if (year_max > max(acquisition_year_breaks)) {
  gap_to_latest <- year_max - max(acquisition_year_breaks)

  if (gap_to_latest <= 2) {
    acquisition_year_breaks <- head(acquisition_year_breaks, -1)
  }

  acquisition_year_breaks <- c(acquisition_year_breaks, year_max)
}

f2 <- latest_file_by_pattern(
  file.path(PATH, "generalized/ALS_coverage_all_*_generalized_v2.gpkg"),
  stamp_regex = "ALS_coverage_all_(\\d{8})_generalized_v2\\.gpkg",
  label = "generalized ALS coverage GPKG"
)

D1 <- st_read(f2)
# D1 <- st_read("K:/OneDrive - NRCan RNCan/_WORK/5_lidarStatusCanada/ALS_coverage_all_2024_generalized_v2.gpkg")
D1$name <- "ALS data coverage"


r <- rast("layers/manage_unmanaged_v2_aggreg.tif")
r <- terra::as.factor(r)


f2 <- latest_file_by_pattern(
  file.path(PATH, "overlap/ALS_coverage_overlap_*.gpkg"),
  stamp_regex = "ALS_coverage_overlap_(\\d{8})\\.gpkg",
  label = "overlap ALS coverage GPKG"
)

O <- st_read(f2)

#exclude delta year of a year or less
O %<>% filter(YEAR_delta > 1)

O$delta_class <- cut(
  O$YEAR_delta,
  breaks = c(1, 2, 5, 10, 15, 99),
  labels = c("1-2", "3-5", "6-10", "11-15", ">15")
)


# additional data
water <- st_read("layers/water.gpkg")
ecozones_forested <- st_read("layers/ecozones_forested.gpkg")
provinces2 <- st_read("layers/provinces2.gpkg")
provinces_labels <- st_read("layers/provinces_labels.gpkg")


the_bbox <- water %>% st_bbox()


# logo_path <- "img/NRCan-CFS_logo.png"
logo_path <- "img/NRCan_CFS_logo_PT_plain.svg"
# logo <- readPNG(logo_path)
logo <- magick::image_read_svg(logo_path, width = 1661, height = 388)
logo_aspect_ratio <- 1661 / 388 #needed when calculating logo position on the map


# credits_text <- "Source: ptompalski.github.io/CanadaForestLidarCoverage. ALS data coverage as of 2024.09.16. Note that map may not be exhaustive."
credits_text <- glue(
  "Source: ptompalski.github.io/CanadaForestLidarCoverage. Map generated on {today()}. Note that map may not be exhaustive."
)

ecozones_forested$Name <- "Forested ecozones"

coords <- provinces_labels %>% sf::st_coordinates()
provinces_labels <- provinces_labels %>% bind_cols(coords)

# # calculating position and size of the logo
# size_x <- as.numeric(the_bbox[3]-the_bbox[1])
# logo_prop <- 0.2
#
# logo_size_x <- size_x * logo_prop
# logo_size_y <- logo_size_x * 11.4/49
#
# logo_xmin <- -2289876
# logo_xmax <- logo_xmin + logo_size_x
#
# logo_ymin <- -704116
# logo_ymax <- logo_ymin + logo_size_y

# mask / crop acquisition polygons with jurisdiction polygons
Dx <- st_intersection(D, provinces2)
#classify point density
Dx$PPM_class <- cut(
  Dx$PPM,
  breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
  labels = c("<1", "1-2", "2-5", "5-10", "10-20", "20-50", ">50")
)


### common map elements ####

#water
WATER <- geom_sf(data = water, fill = "#eff3ff", color = NA)

# forested ecozones (background)
# ECOZONES <- "geom_sf(data=ecozones_forested, aes(fill=factor(Name)), color=NA, alpha=0.25) + scale_fill_manual(values='#c2e699', name=NULL)"
ECOZONES <- geom_sf(
  data = ecozones_forested,
  aes(fill = factor(Name)),
  color = "#d8efbc",
  alpha = 0.25,
  linewidth = 0.25
)
ECOZONES_formatting <- scale_fill_manual(values = '#c2e699', name = NULL)

#ecozones border only for map 1
ECOZONES3 <- geom_sf(data = ecozones_forested, fill = NA, color = "#a1d99b")

#jurisdiction outlines
JURISDICTIONS <- geom_sf(
  data = provinces2,
  fill = NA,
  color = "grey50",
  linewidth = 0.1
)

# jurisdiction labels
LABELS <-
  with_outer_glow(
    geom_sf_text(data = provinces_labels, aes(label = PROV), family = thefont), #fontface="italic" ),
    expand = 2,
    colour = "white",
    sigma = 10
  )

# define extent
EXTENT <- coord_sf(
  xlim = c(the_bbox[1], the_bbox[3]),
  ylim = c(the_bbox[2], the_bbox[4]),
  expand = FALSE
)

# theme formatting
THEME_SETTINGS <- ggthemes::theme_map(base_family = thefont) +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    legend.position = c(0.97, 0.97),
    legend.justification = c(1, 1),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.background = element_rect(fill = NA),
    #increase legend title size
    legend.title = element_text(size = 12)
  )

#scalebar
SCALEBAR <- ggspatial::annotation_scale(
  width_hint = 0.1,
  style = "ticks",
  pad_x = unit(0.25, "in"),
  pad_y = unit(0.3, "in"),
  text_family = thefont,
  location = "br",
  tick_height = 0
)

#credits
CREDITS <- annotate(
  "text",
  label = credits_text,
  x = Inf,
  y = -Inf,
  family = thefont,
  size = 2,
  hjust = 1.05,
  vjust = -1,
  color = "grey40"
)

#add logo
# LOGO <- annotation_raster(raster = logo, xmin = logo_xmin, xmax = logo_xmax, ymin = logo_ymin, ymax = logo_ymax)
