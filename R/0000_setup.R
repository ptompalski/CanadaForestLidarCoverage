library(tidyverse)
library(sf)
library(magrittr)
# library(tmap)
# library(tmaptools)
library(terra)
library(extrafont)
library(viridis)
library(MetBrewer)
library(glue)
library(patchwork)
library(units)
library(ggnewscale)
library(ggspatial)
library(shadowtext)
library(png)
library(tidyterra)
library(ggpattern)
library(knitr)
library(fs)
library(units)
library(lwgeom)
library(smoothr)

source("R/9999_functions.R")
for (function_file in sort(list.files("R/functions", pattern = "\\.R$", full.names = TRUE))) {
  source(function_file)
}

#current date - to append to the file names (data versioning)
ver <- today() %>% str_remove_all(pattern = "-")


the_crs <- 3978

logo_path <- "img/NRCan-CFS_logo.png"

forested_ecozones_path <- "layers/Forested_ecozones_cliped_dissolved.shp"
canada_provinces_path <- "layers/Canada_provinces.shp"
managed_forest_mask_path <- "layers/managed_forest_mask.tif"


fname <- function(x) {
  basename(tools::file_path_sans_ext(x))
}


# Define custom palettes for when there are 1-2, 3, or 4-6 levels
opts <- options(
  ggplot2.discrete.colour = list(
    c("#4575b4", "#66bd63"),
    RColorBrewer::brewer.pal(3, "Set2"),
    c("#4053d3", "#ddb310", "#b51d14", "#00beff", "#fb49b0"),
    RColorBrewer::brewer.pal(6, "Accent")
  )
)
opts <- options(
  ggplot2.discrete.fill = list(
    c("#4575b4", "#66bd63"),
    RColorBrewer::brewer.pal(3, "Set2"),
    c("#4053d3", "#ddb310", "#b51d14", "#00beff", "#fb49b0"),
    RColorBrewer::brewer.pal(6, "Accent")
  )
)


opts

# thefont <- "Segoe UI"
thefont <- "Lato"

theme_clean <- theme_light(base_family = thefont) %+replace%
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = rel(1)),
    panel.grid = element_blank(),
    axis.ticks = element_line(colour = "black", size = rel(0.5)),
    legend.position = "bottom",
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(fill = NA, colour = NA)
  )

ggplot2::theme_set(theme_clean)

provinces_area <-
  tribble(
    ~jurisdiction_name          , ~jurisdiction_code , ~total_area , ~total_forest_area , ~managed_forest_area ,

    "British Columbia"          , "BC"               ,      925186 ,             632864 ,               500534 ,
    "Alberta"                   , "AB"               ,      642317 ,             341261 ,               250011 ,
    "Saskatchewan"              , "SK"               ,      591670 ,             252942 ,                85897 ,
    "Manitoba"                  , "MB"               ,      553556 ,             262938 ,               115529 ,
    "Ontario"                   , "ON"               ,      917741 ,             652763 ,               334150 ,
    "Quebec"                    , "QC"               ,     1365128 ,             809223 ,               380741 ,
    "New Brunswick"             , "NB"               ,       71450 ,              65902 ,                58270 ,
    "Prince Edward Island"      , "PE"               ,        5660 ,               2888 ,                 2493 ,
    "Nova Scotia"               , "NS"               ,       53338 ,              47958 ,                37708 ,
    "Newfoundland and Labrador" , "NL"               ,      373872 ,             197691 ,                62010 ,
    "Yukon Territory"           , "YT"               ,      474391 ,             219441 ,                39312 ,
    "Northwest Territories"     , "NT"               ,     1183085 ,             428262 ,                 8686
  )
