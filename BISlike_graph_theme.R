BISlike_graph_theme <- function() {
  theme(
    text = element_text(family = "Segoe UI"),
    panel.background = element_rect(fill = "#d3d6d4"),
    strip.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#f2f2f2"),
    
    strip.text.y = element_blank(),

    axis.text = element_text(colour = "#a60000", face = "italic"),
    axis.title = element_text(colour = "#a60000"),
    axis.ticks = element_line(colour = "#a60000"),

    legend.position = "bottom"
  )
}
