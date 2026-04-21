library(rmapshaper)

D <- readRDS(glue(
  "layers/ALS_coverage_layer/main/ALS_coverage_all_20251030.rds"
))

# st_write(D, glue("ALS_coverage_all_{ver}.gpkg"), append=F)

#postprocess the acquisition polygons

# clip to provinces vector (no areas outside)
provinces <- st_read(file.path(
  "K:/OneDrive - NRCan RNCan/_WORK",
  "useful_layers",
  "Canada_provinces.shp"
))
provinces2 <- provinces %>%
  group_by(PROV) %>%
  summarise(n = n(), area = sum(Shape_Area)) %>%
  mutate(PROV = toupper(PROV)) %>%
  st_cast()


Dx <- st_intersection(D, provinces2)
# saveRDS(Dx, "layers/ALS_coverage_all_2025_clipped.rds")

# classify point density
Dx$PPM_class <- cut(
  Dx$PPM,
  breaks = c(1, 2, 5, 10, 20, 50, 99),
  labels = c("1-2", "3-5", "6-10", "11-20", "21-50", ">50")
)

D2 <- Dx %>%
  group_by(PPM_class) %>%
  summarise(n = n()) %>%
  st_cast()

D2 <- D2 %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON", warn = FALSE)


#remove holes
library(nngeo)
D2 <- nngeo::st_remove_holes(D2, max_area = 2500000)


D2 <- ms_simplify(D2, keep = 0.02)
plot(st_geometry(D2))

poly <- st_transform(D2, 4326)


leaflet(options = leafletOptions(minZoom = 3, maxZoom = 8)) |>
  addTiles() |>
  addPolygons(
    data = poly,
    weight = 2,
    color = "#2c3e50",
    fillColor = "#2c3e50",
    fillOpacity = 0.3
  ) |>
  addScaleBar()


pal <- colorFactor("Set2", domain = levels(poly$PPM_class)) # categorical palette

leaflet(poly, options = leafletOptions(minZoom = 3, maxZoom = 8)) |>
  addTiles() |>
  addPolygons(
    fillColor = ~ pal(PPM_class),
    color = "#2c3e50",
    weight = 1,
    fillOpacity = 0.6,
    label = ~ glue("PPM class: {PPM_class}"),
    highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
  ) |>
  addLegend(
    position = "bottomright",
    pal = pal,
    values = (poly$PPM_class), # keeps legend order
    title = "Point density"
  )


# by year

D2 <- Dx %>%
  group_by(YEAR) %>%
  summarise() %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON", warn = FALSE)


#remove holes
library(nngeo)
D2 <- nngeo::st_remove_holes(D2, max_area = 2500000)


D2 <- ms_simplify(D2, keep = 0.02)


poly <- st_transform(D2, 4326)

poly <- poly %>% mutate(YEAR = suppressWarnings(as.integer(YEAR)))

# Palette: continuous gradient (viridis). Set na.color transparent so missing years don't show.
pal_year <- colorNumeric(
  palette = "viridis",
  domain = poly$YEAR,
  na.color = "transparent",
  reverse = FALSE # set TRUE if you want newest = lightest
)

# Build map
leaflet(poly, options = leafletOptions(minZoom = 3, maxZoom = 8)) |>
  addTiles() |>
  addPolygons(
    fillColor = ~ pal_year(YEAR),
    color = "#2c3e50",
    weight = 1,
    fillOpacity = 0.7,
    label = ~ glue("Acquisition year: {YEAR}"),
    highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
  ) |>
  addLegend(
    pal = pal_year,
    values = ~YEAR,
    title = "Acquisition year",
    opacity = 1
  )

# # st_write(D1, glue("ALS_coverage_all_{ver}_generalized_v2.gpkg"), append=F)
# st_write(
#   D1,
#   glue(
#     "layers/ALS_coverage_layer/generalized/ALS_coverage_all_{ver}_generalized_v2.gpkg"
#   ),
#   append = F
# )

# # r <- rast("layers/manage_unmanaged_v2_aggreg.tif")
# r <- terra::as.factor(r)

# p <- as.polygons(r, dissolve = TRUE, na.rm = TRUE)
# p
# plot(p)
# p <- st_as_sf(p)
# st_write(p, "layers/manage_unmanaged_v2_aggreg.gpkg")

# D1$name <- " "

# map_coverage_forFocusedMaps <-
#   ggplot() +
#   ggthemes::theme_map(base_family = thefont) +
#   WATER +

#   geom_sf(data = p, aes(fill = (managed_forest_mask)), color = NA) +
#   scale_fill_manual(
#     name = "Forest type:",
#     values = c("#41AB5D", "#A1D99B"),
#     labels = c("Managed", "Unmanaged"),
#     guide = guide_legend(
#       order = 2,
#       nrow = 1,
#       title.position = "left",
#       title.hjust = 0,
#       legend.title = element_text(size = 7),
#       legend.text = element_text(size = 7),
#       legend.key.size = unit(1, "lines")
#     )
#   ) +

#   # geom_spatraster(data = r, aes(fill = managed_forest_mask), maxcell = 5e+08) +
#   # scale_fill_coltab(
#   #   data = r,
#   #   name = "Forest type:",
#   #   labels = c("Managed", "Unmanaged"),
#   #   guide = guide_legend(
#   #     order = 2,
#   #     nrow = 1,
#   #     title.position = "left",
#   #     title.hjust = 0,
#   #     legend.title = element_text(size = 7),
#   #     legend.text = element_text(size = 7),
#   #     legend.key.size = unit(1, "lines")
#   #   )
#   # ) +
#   JURISDICTIONS +
#   geom_sf_pattern(
#     data = D1,
#     aes(pattern = name),
#     color = "black",
#     linewidth = 0.15,
#     fill = NA,
#     pattern_fill = "black",
#     pattern_fill2 = NA,
#     pattern_frequency = 1,
#     pattern_size = 0.005,
#     pattern_density = 0.05,
#     pattern_spacing = 0.005
#   ) +
#   scale_pattern_discrete(
#     name = "ALS data coverage",
#     guide = guide_legend(
#       order = 1,
#       legend.text = element_blank(),
#       legend.box.margin = margin(t = 4, unit = 'cm')
#     )
#   ) +
#   LABELS +
#   EXTENT +
#   THEME_SETTINGS2
# ggsave(map_coverage_forFocusedMaps, filename = "coverage_test.png")
