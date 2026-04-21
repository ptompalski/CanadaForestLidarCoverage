source("R/2000_maps_setup.R")

EXTENT2 <- coord_sf(xlim = c(the_bbox[1], the_bbox[3]), 
                   ylim = c(the_bbox[2]+249296, the_bbox[4] - 498592.1),
                   expand = FALSE)

THEME_SETTINGS2 <- ggthemes::theme_map(base_family = thefont, base_size = 16)+
  theme(plot.background = element_rect(fill="white",color = "white"),
        legend.position =c(0.97,0.97), 
        legend.justification=c(1,1),
        legend.spacing.y = unit(-0.2, "cm"),
        legend.background = element_rect(fill = NA),
        #increase legend title size
        legend.title = element_text(size=18)
  )




# with DEM 
#data

library(ggh4x)
library(ggnewscale)
library(grid)
library(rmapshaper)
library(ggspatial)
library(patchwork)

ecozones <- st_read("../useful_layers/Ecozones/forested_ecozones_disolved_erased.shp")
prov <- st_read("../useful_layers/ca_prov.shp")

ecozones %<>% st_transform(crs=st_crs(D_dissolved))
prov %<>% st_transform(crs=st_crs(D_dissolved))

ecozones_simplified <- ms_simplify(ecozones, keep = 0.01)


water <- st_read("F:/useful_layers/water.shp")
water <- sf::st_transform(water, st_crs(D_dissolved))

NorthAmerica <- st_read("F:/useful_layers/North_america_prov_states.shp")
NorthAmerica <- sf::st_transform(NorthAmerica, st_crs(D_dissolved))

# DEM <- rast("F:/useful_layers/Elevation_GRID/NA_Elevation_forestedEcozones.tif")
# hillshade <- rast("F:/useful_layers/Elevation_GRID/NA_ElevationHillshade_forestedEcozones.tif")
hillshade <- rast("F:/useful_layers/Elevation_GRID/hillshade_1km_LCC.tif")

#treed 
treed <- rast("K:/NTEMS/temp/Canada_treed_pixels_2022_resamp_1km_masked.tif")
treed <- as.factor(treed)

water_pixels <- rast("K:/NTEMS/temp/Canada_water_pixels_2022_resamp_1km_masked.tif")
water_pixels <- as.factor(water_pixels)

#managed / unmanaged

r <- rast("../CanadaForestLidarCoverage/layers/manage_unmanaged_v2_aggreg.tif")
r <- terra::as.factor(r)


# inverted ecozones polygon
polygon <- st_as_sf(as.polygons(ext(hillshade), crs = crs(hillshade))) 
#(cut hole in the tiles using ecozones)
ecozones_simplified_inverted <- st_difference(polygon, ecozones_simplified)


# remove Ontario from multi-temporal polygon layer - those overlaps are wrong
O <- O %>% filter(Province != "ON")



overview_col <- "#1a9850"
multitemporal_col <- "#ffeda0"

library(tidyterra)

p <- ggplot(data=NULL) +
  geom_sf(data=NorthAmerica, color=NA, fill="white")+
  
  layer_spatial(data=hillshade, alpha=0.75) + scale_fill_gradient(low = "grey10", high = "grey95")+
  
  geom_sf(data=water, color=NA, fill="#deebf7")+
  
  ggnewscale::new_scale_fill()+
  layer_spatial(data=water_pixels, alpha=0.5) + scale_fill_manual(na.value = NA, values = "#deebf7")+
  
  ggnewscale::new_scale_fill()+
  
  geom_sf(data=D_dissolved, aes(fill=factor(Name)), color=NA, alpha=0.65)+
  scale_fill_manual(values=overview_col, name=NULL)+
  
  geom_sf(data=ecozones_simplified, color="#559157", fill=NA)+

  #mask the outside of forested eco
  geom_sf(data=ecozones_simplified_inverted, fill="white", alpha=0.5)+
  
  JURISDICTIONS+

  EXTENT2+
  
  theme_bw(base_size = 11, base_family = thefont)+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # axis.text.y = element_text(angle = 90, hjust = 0.5),
    panel.grid = element_line(color="grey80", linewidth = 0.5),
    legend.position = "none"
  )
fout <- "../Slides/Silvilaser2025_Keynote/figures/map_test1.png"
ggsave(plot = p, filename = fout, width = 16, height=9, dpi = 300)




p <- ggplot(data=NULL) +
  geom_sf(data=NorthAmerica, color=NA, fill="white")+
  
  layer_spatial(data=hillshade, alpha=0.75) + scale_fill_gradient(low = "grey10", high = "grey95")+
  
  geom_sf(data=water, color=NA, fill="#deebf7")+
  
  ggnewscale::new_scale_fill()+
  layer_spatial(data=water_pixels, alpha=0.5) + scale_fill_manual(na.value = NA, values = "#deebf7")+
  
  ggnewscale::new_scale_fill()+
  
  geom_sf(data=D_dissolved, aes(fill=factor(Name)), color=NA, alpha=0.65)+
  scale_fill_manual(values=overview_col, name=NULL)+
  
  geom_sf(data=O, fill=multitemporal_col,color=NA, alpha=0.75)+
  
  geom_sf(data=ecozones_simplified, color="#559157", fill=NA)+
  
  #mask the outside of forested eco
  geom_sf(data=ecozones_simplified_inverted, fill="white", alpha=0.5)+
  
  JURISDICTIONS+
  
  EXTENT2+
  
  theme_bw(base_size = 11, base_family = thefont)+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # axis.text.y = element_text(angle = 90, hjust = 0.5),
    panel.grid = element_line(color="grey80", linewidth = 0.5),
    legend.position = "none"
  )
fout <- "../Slides/Silvilaser2025_Keynote/figures/map_test2.png"
ggsave(plot = p, filename = fout, width = 16, height=9, dpi = 300)




# with DEM overview map for the poster
ecozones_simplified$Name <- "Forested ecozones"

p <- ggplot(data=NULL) +
  geom_sf(data=NorthAmerica, color=NA, fill="white")+
  
  layer_spatial(data=hillshade, alpha=0.75) + scale_fill_gradient(low = "grey10", high = "grey95",guide = "none")+
  
  geom_sf(data=water, color=NA, fill="#deebf7")+
  
  ggnewscale::new_scale_fill()+
  layer_spatial(data=water_pixels, alpha=0.5) + scale_fill_manual(na.value = NA, values = "#deebf7",guide = "none")+
  
  ggnewscale::new_scale_fill()+
  
  geom_sf(data=D_dissolved, aes(fill=factor(Name)), color=NA, alpha=0.65)+
  scale_fill_manual(values=overview_col)+
  
  # geom_sf(data=ecozones_simplified, color="#559157", fill=NA)+
  geom_sf(data=ecozones_simplified, aes(color=factor(Name)), fill=NA, linewidth=1)+
  scale_color_manual(values='grey20',  
                     guide = guide_legend(override.aes = list(      fill = "grey50",        # transparent interior in legend
                                                                    color = "grey20", # keep grey20 border
                                                                    alpha = 0.5)))+
  
  #mask the outside of forested eco
  geom_sf(data=ecozones_simplified_inverted, fill="white", alpha=0.5)+
  
  # guides(color = guide_legend(override.aes = list(alpha = 0.3)))+
  
  JURISDICTIONS+
  LABELS+
  EXTENT+
  annotate("text", label=credits_text, x=Inf, y=-Inf, family=thefont, size=5, hjust=1.05,vjust=-1, color="grey40")+
  theme_bw(base_size = 28, base_family = thefont)+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # axis.text.y = element_text(angle = 90, hjust = 0.5),
    panel.grid = element_line(color="grey80", linewidth = 0.5),
    legend.position =c(0.97,0.97), 
    legend.justification=c(1,1),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.background = element_rect(fill = NA),
    #increase legend title size
    legend.title = element_blank(),#element_text(size=12)
    legend.text = element_text(size = 32),
    legend.key.width  = unit(1, "cm"),
    legend.key.height = unit(1, "cm")
    # legend.position = "none"
  )
fout <- "../Slides/Silvilaser2025_Keynote/figures/map_poster.png"
ggsave(plot = p, filename = fout, width = 56.63, height=40.45, dpi = 300, units = "cm")

#add logo using magick
background <- image_read(fout)

logo
logo_resized <- magick::image_resize(logo, geometry = "1500x")

newImg <- image_composite(background, logo_resized, gravity = "SouthWest", offset = "+150+180")
# newImg
fout2 <- "../Slides/Silvilaser2025_Keynote/figures/map_poster2.png"
image_write(newImg, fout2)

#as tif
fout <- "../Slides/Silvilaser2025_Keynote/figures/map_poster.tif"
ggsave(plot = p, filename = fout, width = 56.63, height=40.45, dpi = 300, units = "cm")

#add logo using magick
background <- image_read(fout)

logo
logo_resized <- magick::image_resize(logo, geometry = "1500x")

newImg <- image_composite(background, logo_resized, gravity = "SouthWest", offset = "+150+180")
# newImg
image_write(newImg, fout)


# ECOZONES <- geom_sf(data=ecozones_forested, aes(fill=factor(Name)), color="#d8efbc", alpha=0.25, linewidth=0.25) 
# ECOZONES_formatting <- scale_fill_manual(values='#c2e699', name=NULL)


# plot(st_geometry(D_dissolved))
# 
# 
# 
# pol_clean <- D_dissolved %>%
#   st_make_valid() %>%               # fix invalid geometries
#   st_buffer(0) %>%                  # common trick to snap/remove slivers
#   st_union() %>%                    # dissolve into single geometry
#   st_collection_extract("POLYGON")  # break back into polygons
# 
# # remove polygons smaller than threshold (e.g., 100 m²)
# min_area <- units::set_units(100, "m^2")
# 
# pol_clean <- pol_clean %>%
#   filter(st_area(.) > min_area)