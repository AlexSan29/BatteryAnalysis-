Theme_AS <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      panel.border = element_rect(
        fill = NA,
        color = "black",
        linewidth = 0.8
      ),
      
      panel.grid.major = element_line(
        color = "#D9D9D9",
        linewidth = 0.6
      ),
      panel.grid.minor = element_blank(),
      
      axis.text = element_text(
        size = 12,
        color = "black"
      ),
      axis.title = element_text(
        size = 14,
        color = "black"
      ),
      
      axis.ticks = element_line(
        color = "black",
        linewidth = 0.5
      ),
      axis.ticks.length = unit(0.15, "cm"),
      
      plot.title = element_text(
        size = 16,
        face = "bold",
        color = "black",
        hjust = 0,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 14,
        color = "gray40",
        hjust = 0,
        margin = margin(b = 8)
      ),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.key = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
      
      strip.text = element_text(
        size = 14,
        face = "bold",
        color = "black"
      ),
      
      plot.caption = element_text(
        size = 12,
        color = "black",
        hjust = 0,
        margin = margin(t = 6)
      ),
      
      plot.title.position = "panel",
      plot.caption.position = "plot"
    )
}