source("R/2000_maps_setup.R")
# library(colorspace)
### MAPS for the paper ####


# define extent

EXTENT <- coord_sf(xlim = c(the_bbox[1], the_bbox[3]), 
                   ylim = c(the_bbox[2]+300000, the_bbox[4]),
                   expand = FALSE)


fig_ratio = 1.554
fig_width=19
fig_height=fig_width/fig_ratio

#### map1 - overall coverage ####

r <- rast("layers/manage_unmanaged_v2_aggreg.tif")
D1 <- st_read("C:/Users/ptompals/OneDrive - NRCan RNCan/__WORK/5_lidarStatusCanada/ALS_coverage_all_2024_generalized_v2.gpkg")
D1$name <- "ALS data coverage"
r <- terra::as.factor(r)


map_coverage <-
  ggplot() + 
  WATER+
  ECOZONES3 +
  geom_spatraster(data=r,aes(fill=managed_forest_mask))+
  scale_fill_coltab(data=r, name="Forest type", labels=c("Managed", "Unmanaged"))+
  
  JURISDICTIONS +
  
  geom_sf_pattern(data=D1, aes(pattern = name), 
                  color="black", 
                  linewidth=0.15,
                  fill=NA, 
                  pattern_fill="black", 
                  pattern_fill2=NA, 
                  pattern_frequency=1,
                  pattern_size=0.005,
                  pattern_density=0.05,
                  pattern_spacing=0.005
  )+
  
  scale_pattern_discrete(name=NULL)+
  LABELS +
  EXTENT + THEME_SETTINGS + SCALEBAR + 
  theme(legend.title = element_text(size=10)) #+ CREDITS# +LOGO


fout <- "img/map1_ALS_coverage_paper.tif"
ggsave(plot = map_coverage, filename = fout, width = fig_width, height=fig_height, units="cm", dpi=300)







#### map2 - density ####

map_density <- 
  ggplot(data=NULL) + WATER + 
  ECOZONES + ECOZONES_formatting+
  new_scale_fill()+
  
  geom_sf(data=Dx, aes(fill=PPM_class), color=NA)+
  
  JURISDICTIONS+LABELS+
  
  scale_fill_manual(
    values =  as.character(MetBrewer::met.brewer(name = "Hokusai2", n = 7)),
    name = "ALS data density",
    na.translate = FALSE,
    guide = guide_legend(ncol = 2,
                         order = 1,
                         title.position = "top",
                         reverse = F 
    )
  )+
  EXTENT + THEME_SETTINGS + SCALEBAR 

# map_density
fout <- "img/map2_ALS_density_paper.tif"
ggsave(plot = map_density, filename = fout, width = fig_width, height=fig_height, units="cm", dpi=300)






#### map3 - acquisition year ####

map_acquisitionYear <- 
  ggplot(data=NULL) + 
  WATER + ECOZONES + ECOZONES_formatting + new_scale_fill()+
  
  geom_sf(data=Dx, aes(fill=(YEAR)), color=NA)+
  
  JURISDICTIONS + LABELS +
  
  scale_fill_gradientn(
    name="Acquisition year",
    colours = rev(as.character(MetBrewer::met.brewer(name = "Demuth", n = length(unique(D$YEAR))))),
    breaks=c(2005, 2010, 2015, 2020),
    guide = guide_colorbar(
      order=1, 
      theme = theme(legend.direction = "horizontal", legend.key.width = unit(1.5, "inches")),
      ticks.colour = "black",
      # draw.ulim = FALSE, 
      # draw.llim = FALSE,
      title.position = "top")
  )+
  
  EXTENT+THEME_SETTINGS+
  theme(legend.box.just = "right",
        legend.title = element_text(hjust = 0.5),
        legend.spacing.y = unit(0, "cm"))+
  
  SCALEBAR 

fout <- "img/map3_ALS_AcquisitionYear_paper.tif"
ggsave(plot = map_acquisitionYear, filename = fout, width = fig_width, height=fig_height, units="cm", dpi=300)






#### map4 - overlap areas ####

O <- st_read("C:/Users/ptompals/OneDrive - NRCan RNCan/__WORK/5_lidarStatusCanada/ALS_coverage_multitemporal_v2.gpkg")

#exclude delta year of a year or less
O %<>% filter(YEAR_delta > 1)

O$delta_class <- cut(O$YEAR_delta, breaks=c(1,2,5,10,15,20), labels=c("1-2", "2-5", "5-10", "10-15",">15"))

map_overlap <- 
  ggplot(data=NULL) + 
  WATER + ECOZONES + ECOZONES_formatting + new_scale_fill()+
  
  geom_sf(data=O, aes(fill=(delta_class)), color=NA)+
  
  JURISDICTIONS + LABELS +
  
  scale_fill_manual(
    values = viridis::magma(5),
    name = "Years between overlapping acquisitions",
    guide = guide_legend(ncol = 5,
                         order = 1,
                         title.position = "top",
                         reverse = F 
    ))+
  
  EXTENT+THEME_SETTINGS+
  theme(legend.box.just = "right",
        legend.title = element_text(hjust = 0.5),
        legend.spacing.y = unit(0.2, "cm"))+
  
  SCALEBAR

# ggsave(plot = map_overlap, filename = "img/map4_ALS_overlap.tif", width = 7, height=5)
fout <- "img/map4_ALS_overlap_paper.tif"
ggsave(plot = map_overlap, filename = fout, width = fig_width, height=fig_height, units="cm", dpi=300)

