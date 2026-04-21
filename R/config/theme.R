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
