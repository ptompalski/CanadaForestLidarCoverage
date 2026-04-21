# maps / graphics

# library(extrafont)
# # font_import()
# loadfonts(device = "win")

PATH <- "C:/Users/ptompals/OneDrive - NRCan RNCan/__WORK"

source("R/0000_setup.R")


# ALS coverage
D <- readRDS("layers/ALS_coverage_all_2024.rds")

#make sure year is a number
D$YEAR <- as.numeric(D$YEAR)


# additional data
water <- st_read("layers/water.gpkg")
ecozones_forested <- st_read("layers/ecozones_forested.gpkg")
provinces2 <- st_read("layers/provinces2.gpkg")
provinces_labels <- st_read("layers/provinces_labels.gpkg")
bbox_layer <- st_read("layers/bbox_layer.shp")

NA_hillshade <- rast("F:/useful_layers/NA_hillshade/srgrayi1kml.tif")
NA_hillshade <- terra::project(NA_hillshade, crs(water))

# the_bbox <- provinces2 %>% filter(!(PROV %in% c("NT","NU"))) %>% st_bbox()
the_bbox <- bbox_layer %>% st_bbox()
# the_bbox <- st_bbox(c(xmin = -2340995.0, xmax = 3010720.2, ymax = 2840065, ymin = -335174), crs = st_crs(water)) 

#ratios for defining output graphic size
ratio1 <- 1.496
ratio2 <- 1.175
fig_width = 3000


credits_text <- "ALS data coverage as of 2024.03.31. Note that map may not be exhaustive."

hatch <- function(x, density) {
  # x: polygon object (SpatialPolygons* or sf)
  # density: approx number of lines to plot
  require(sp)
  require(raster)
  e <- extent(x)
  w <- diff(e[1:2])
  x1 <- seq(xmin(e), xmax(e)+w, length.out=floor(density*2))
  x0 <- seq(xmin(e)-w, xmax(e), length.out=floor(density*2))
  y0 <- rep(ymin(e), floor(density*2))
  y1 <- rep(ymax(e), floor(density*2))
  ll <- spLines(mapply(function(x0, y0, x1, y1) {
    rbind(c(x0, y0), c(x1, y1))
  }, x0, y0, x1, y1, 
  SIMPLIFY=FALSE))  
  if(is(x, 'sf')) {
    require(sf)
    ll <- st_as_sf(ll)
    st_crs(ll) <- st_crs(x)
    st_intersection(ll, x)
  } else {
    proj4string(ll) <- proj4string(x)
    raster::intersect(ll, x)
  }
}


# maps
# manage_unmanaged_v2 <- rast("//vic-fas1/france/cwfc/forest_inventory_status/spatial_data/managed_forest_mask.tif")
# 
# coltb <- data.frame(value=c(1,2), col=c("#41ab5d","#a1d99b"))
# coltab(manage_unmanaged_v2) <- coltb
# 
# r <- terra::aggregate(manage_unmanaged_v2, fact=4, fun="modal", na.rm=TRUE)
# writeRaster(r, "layers/manage_unmanaged_v2_aggreg.tif")

r <- rast("layers/manage_unmanaged_v2_aggreg.tif")

D1 <- st_read("layers/ALS_coverage_all_2024_generalized.gpkg")

h <- hatch(D1, 60)


p<-ggplot(tibble(x=c(0,1),y=c(0,1)), aes(x,y)) + 
  # geom_abline()+
  geom_abline(slope = 0.6, intercept = 0.7)+
  geom_abline(slope = 0.6, intercept = 0.2)+
  geom_abline(slope = 0.6, intercept = -0.3)+
  theme_ps(plot.axes = FALSE) +
  coord_fixed(expand = F, xlim = c(0,1), ylim = c(0,1))
p <- ggplotGrob(p)

data(NLD_prov)
origin_data <- NLD_prov %>% 
  st_set_geometry(NULL) %>% 
  mutate(FID= factor(1:n())) %>% 
  as_tibble() %>%
  dplyr::select(FID, origin_native, origin_west, origin_non_west) %>% 
  gather(key=origin, value=perc, origin_native, origin_west, origin_non_west, factor_key=TRUE)

origin_cols <- get_brewer_pal("Dark2", 3)

grobs <- lapply(split(origin_data, origin_data$FID), function(x) {
  ggplotGrob(ggplot(x, aes(x="", y=-perc, fill=origin)) +
               geom_bar(width=1, stat="identity") +
               scale_y_continuous(expand=c(0,0)) +
               scale_fill_manual(values=origin_cols) +
               theme_ps(plot.axes = FALSE))
})
names(grobs) <- NLD_prov$name

grobs$Groningen <- p
NLD_prov1 <- NLD_prov %>% filter(name=="Groningen")


M1a <- 
  tm_shape(water, bbox = the_bbox) + tm_fill(col = "#eff3ff")+
  
  #managed/unmanaged
  tm_shape(r) +
  tm_raster(palette = c("#41ab5d","#c7e9c0"), 
            style = "cat", 
            legend.show = T, 
            labels = c("Managed", "Unmanaged"),
            title = "Forest type"
  )+
  tm_shape(h)+tm_lines(lwd = 0.7)+
  tm_shape(D1)+tm_borders(lwd = 1, col = "black")+
  tm_shape(provinces2) + tm_borders(col = "black", alpha = 0.5, lwd = 0.5) +
  # tm_shape(provinces3) + tm_borders(col = "black", alpha = 0.5, lwd = 0.5) +
  tm_shape(provinces_labels) + tm_text(text = "PROV", fontfamily = thefont, shadow=T, fontface="bold.italic", size=1.2, col = "black")+
  
  tm_scale_bar(breaks = c(0, 500, 1000)) +  
  # tm_add_legend(type = "fill", labels = "ALS data coverage", col = "white", border.col = "black")+
  # tm_add_legend(type = "symbol", labels = "ALS data coverage", shape=p)+
  
  #logo
  tm_logo(logo_path,height=3, position = c("left", "bottom"))+
  
  #scale
  tm_scale_bar(breaks = c(0, 500, 1000))+
  
  #credits
  tm_credits(text = credits_text, size=0.5, alpha = 0.5)+
  
  tm_layout(legend.position = c("right", "top"),  legend.width = 0.4)+
  
  tm_shape(NLD_prov1) +
  tm_symbols(size="population",
             shape="name",
             shapes=grobs,
             sizes.legend=c(1)*1e5, border.col = "black",
             
             sizes.legend.labels = "ALS data coverage",
             title.size = "",
             scale=1,
             legend.shape.show =F,
             legend.size.is.portrait = TRUE,
  )

tmap_save(tm = M1a, filename = "img/map1_ALS_coverage.png", width = fig_width, height = round(fig_width / ratio1))


# # map - ALS coverage
# M3 <- 
#   #water + define the bbox
#   tm_shape(water, bbox = the_bbox) + tm_fill(col = "#eff3ff")+
#   
#   # forested ecozones
#   tm_shape(ecozones_forested) + tm_fill(col = "#c2e699", alpha = 0.5)+
#   
#   # ALS coverage
#   tm_shape(D)+  tm_fill(col = "#80b1d3")+
#   
#   # p/t borders and labels
#   tm_shape(provinces2) + tm_borders(col = "black", alpha = 0.5, lwd = 0.5) +
#   tm_shape(provinces_labels) + tm_text(text = "PROV", fontfamily = thefont, shadow=T)+
#   
#   #logo
#   tm_logo(logo_path,height=3, position = c("left", "bottom"))+
#   
#   #title
#   tm_layout(title = "ALS data coverage", title.fontface = "bold")+
#   
#   #scale
#   tm_scale_bar(breaks = c(0, 500, 1000))+
#   
#   #credits
#   tm_credits(text = credits_text, size=0.5, alpha = 0.5)
# M3
# tmap_save(tm = M3, filename = "img/map1_ALS_coverage.png", width = fig_width, height = round(fig_width / ratio1))  


# map - ALS density
# styleargs <- classInt::classIntervals(D$PPM, style="fixed", fixedBreaks=c(0, 1,2,5,10,20,50))
# D %>% mutate(PPM_class = )
D$PPM_class <- cut(D$PPM, breaks=c(0, 1,2,5,10,20,50,100), labels=c("<1","1-2", "2-5", "5-10", "10-20", "20-50", ">50"))


M4 <- tm_shape(water, bbox = the_bbox) + tm_fill(col = "#eff3ff")+
  # forested ecozones
  tm_shape(ecozones_forested) + tm_fill(col = "#c2e699", alpha = 0.25)+
  # ALS coverage
  tm_shape(D)+ tm_fill(col = "PPM_class", 
                       # breaks = c(1,2,5,10,20,30,50), 
                       # style="cont",
                       style="cat", 
                       title="ALS data density",
                       
                       palette = as.character(MetBrewer::met.brewer("Hokusai2")))   +
  # p/t borders and labels
  tm_shape(provinces2) + tm_borders(col = "black", alpha = 0.5, lwd = 0.5 ) +
  tm_shape(provinces_labels) + tm_text(text = "PROV", fontfamily = thefont, shadow=T)+
  
  tm_logo(logo_path,height=3, position = c("left", "bottom"))+
  # tm_layout(title = "ALS data density", title.fontface = "bold")+
  
  
  tm_scale_bar(breaks = c(0, 500, 1000))+
  tm_credits(text = credits_text, size=0.5, alpha = 0.5)+
  tm_add_legend(type="fill", col = "#c2e699", alpha = 0.25, labels = "Forested ecozones")+
  tm_layout(legend.position = c("right", "top"), legend.just="right")

tmap_save(tm = M4, filename = "img/map2_ALS_density.png", width = fig_width, height = round(fig_width / ratio1))  


# map5 - ALS acquisition year
colors_acquisition <- rev(as.character(MetBrewer::met.brewer("Demuth",n = 20)))

M5 <- tm_shape(water, bbox = the_bbox) + tm_fill(col = "#eff3ff")+
  # forested ecozones
  tm_shape(ecozones_forested) + tm_fill(col = "#c2e699", alpha = 0.25)+
  # ALS coverage
  tm_shape(D)+
  tm_fill(col = "YEAR",
          style="cat",
          # style="cont",
          title="ALS acquisition year",#breaks = c(2004, 2010, 2020, 2023),
          # labels = c("2004", "2010", "2020", "2023"),
          big.num.abbr=NA,
          legend.is.portrait = T,
          palette = rev(as.character(MetBrewer::met.brewer("Demuth"))))   +

  # p/t borders and labels
  tm_shape(provinces2) + tm_borders(col = "black", alpha = 0.5, lwd = 0.5) +
  tm_shape(provinces_labels) + tm_text(text = "PROV", fontfamily = thefont, shadow=T)+
  
  # tm_logo(logo_path,height=3, position = c("left", "bottom"))+
  # tm_layout(title = "ALS data acquisition year", title.fontface = "bold", title.size = 1.2)+
  tm_scale_bar(breaks = c(0, 500, 1000))+
  tm_credits(text = credits_text, size=0.5, alpha = 0.5)+
  tm_add_legend(type="fill", col = "#c2e699", alpha = 0.25, labels = "Forested ecozones")+
  tm_layout(legend.position = c("right", "top"), legend.width = 0.25)

tmap_save(tm = M5, filename = "img/map3_ALS_AcquisitionYear.png", width = fig_width, height = round(fig_width / ratio1))  




# tm_shape(water, bbox = the_bbox) + tm_fill(col = "#eff3ff")+
#   # forested ecozones
#   tm_shape(ecozones_forested) + tm_fill(col = "#c2e699", alpha = 0.25)+
# 
#   tm_add_legend(type="fill", col = "#c2e699", alpha = 0.25, labels = "Forested ecozones")+
#   
#   tm_add_legend(type = "fill", 
#                 labels = c("2004","2005"), 
#                 col = colors_acquisition[1:2],
#                 is.portrait = F, 
#                 legend.format = list(text.align = "right"))+
#   tm_layout(legend.position = c("right", "top"), legend.width = 0.25)



#with hillshade

# map - ALS coverage
M1h <- 
  tm_shape(NA_hillshade, bbox = the_bbox, raster.downsample = F)+
  tm_raster(palette = grey.colors(10), legend.show = F, alpha=0.5)+

  tm_shape(water, bbox = the_bbox) + tm_fill(col = "#eff3ff")+
  
  #managed/unmanaged
  tm_shape(r) +
  tm_raster(palette = c("#41ab5d","#c7e9c0"), 
            style = "cat", 
            legend.show = T, 
            labels = c("Managed", "Unmanaged"),
            title = "Forest type",
            alpha=0.25
  )+
  tm_shape(h)+tm_lines(lwd = 0.7)+
  tm_shape(D1)+tm_borders(lwd = 1, col = "black")+
  tm_shape(provinces2) + tm_borders(col = "black", alpha = 0.5, lwd = 0.5) +
  # tm_shape(provinces3) + tm_borders(col = "black", alpha = 0.5, lwd = 0.5) +
  tm_shape(provinces_labels) + tm_text(text = "PROV", fontfamily = thefont, shadow=T, fontface="bold.italic", size=1.2, col = "black")+
  
  tm_scale_bar(breaks = c(0, 500, 1000)) +  
  # tm_add_legend(type = "fill", labels = "ALS data coverage", col = "white", border.col = "black")+
  # tm_add_legend(type = "symbol", labels = "ALS data coverage", shape=p)+
  
  #logo
  tm_logo(logo_path,height=3, position = c("left", "bottom"))+
  
  #scale
  tm_scale_bar(breaks = c(0, 500, 1000))+
  
  #credits
  tm_credits(text = credits_text, size=0.5, alpha = 0.5)+
  
  tm_layout(legend.position = c("right", "top"),  legend.width = 0.4)+
  
  tm_shape(NLD_prov1) +
  tm_symbols(size="population",
             shape="name",
             shapes=grobs,
             sizes.legend=c(1)*1e5, border.col = "black",
             
             sizes.legend.labels = "ALS data coverage",
             title.size = "",
             scale=1,
             legend.shape.show =F,
             legend.size.is.portrait = TRUE,
  )
  
  
tmap_save(tm = M1h, filename = "img/map1_ALS_coverage_hillshade.png", width = fig_width, height = round(fig_width / ratio1))  
