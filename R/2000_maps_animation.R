#### map - animation, acquisitions over time ####

# using data with acquisitions by year
flist2 <- dir_info(
  file.path(PATH, "multitemporal"),
  glob = "*.gpkg$",
  recurse = FALSE
)
# get the newest - this file will be used in almost all processing
f2 <- flist2 %>% arrange(desc(modification_time)) %>% slice(1) %>% pull(path)
Q <- st_read(f2)

Q$YEAR <- as.numeric(Q$YEAR)
all_years <- sort(unique(Q$YEAR))

# PPM_class as factor and sorted
Q$PPM_class <- factor(
  Q$PPM_class,
  levels = c("<1", "1-2", "2-5", "5-10", "10-20", "20-50", ">50")
)


for (current_year in all_years) {
  print(current_year)

  Dx_current <- Q %>% filter(YEAR <= current_year)

  map_x <-
    ggplot(data = NULL) +
    WATER +

    # ECOZONES + ECOZONES_formatting+
    geom_sf(
      data = ecozones_forested,
      fill = '#c2e699',
      color = "#d8efbc",
      alpha = 0.25,
      linewidth = 0.25,
      show.legend = F
    ) +
    # geom_sf(data=ecozones_forested, aes(fill=factor(Name)), color="#d8efbc", alpha=0.25, linewidth=0.25, show.legend=TRUE) +
    # scale_fill_manual(values='#c2e699', name=NULL)+

    new_scale_fill() +

    geom_sf(
      data = Dx_current,
      aes(fill = PPM_class),
      color = NA,
      show.legend = TRUE,
      inherit.aes = F
    ) +

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
      ),
      drop = F
    ) +
    # annotate("text", x=-Inf, y=Inf, label = current_year, hjust="inward", vjust="inward", family=thefont, size=14)+
    geom_label(
      data = NULL,
      aes(x = -Inf, y = Inf, label = current_year),
      hjust = "inward",
      vjust = "inward",
      family = thefont,
      size = 8,
      inherit.aes = F
    ) +
    EXTENT +
    THEME_SETTINGS +
    SCALEBAR +
    CREDITS #+LOGO

  # map_density
  fout <- glue("temp/map_animation_{current_year}.png")
  ggsave(plot = map_x, filename = fout, width = 7, height = 5, dpi = 300)

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
}


# make a gif

png_files = list.files("temp", full.names = T)

png_files <- c(png_files, rep(tail(png_files, 1), 10)) # Adjust repetition count

gifski::gifski(
  png_files = png_files,
  gif_file = "img/animation_ALS_over_time.gif",
  loop = T,
  delay = 0.2,
  width = 1097,
  height = 784
)

# library(magick)
#
# # Read PNG files into magick
# img_list <- lapply(list.files("temp", full.names = T), image_read)
#
# # Join images and set higher resolution
# animation <- image_animate(image_join(img_list), fps = 2)  # Adjust FPS for smoother animation
#
# # Save as high-quality GIF
# image_write(animation, path = "img/animation_ALS_over_time.gif", depth = 16, quality = 100, width=800, height=600)
