# =============================================================================
# ThemeAS.R
# Default publication-style ggplot2 theme.
#
# Individual plotting functions can override any setting by adding another
# theme() call after Theme_AS().
# =============================================================================

Theme_AS <- function(BaseFamily = "Arial",
                     LegendPosition = c(0.97, 0.05),
                     LegendJustification = c(1, 0)) {

  theme_classic(base_family = BaseFamily, base_size = 10) +
    theme(
      # -----------------------------------------------------------------------
      # White figure and plotting backgrounds
      # -----------------------------------------------------------------------
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),

      # Full black border around the plotting area
      panel.border = element_rect(
        fill = NA,
        color = "black",
        linewidth = 0.7
      ),

      # No gridlines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # theme_classic() draws separate left and bottom axis lines. The panel
      # border already supplies all four sides, so these are removed.
      axis.line = element_blank(),

      # -----------------------------------------------------------------------
      # Axis text and titles
      # -----------------------------------------------------------------------
      axis.text = element_text(
        size = 10,
        color = "black",
        face = "plain"
      ),
      axis.text.x = element_text(
        margin = margin(t = 7)
      ),
      axis.text.y = element_text(
        margin = margin(r = 7)
      ),
      axis.title = element_text(
        size = 12,
        color = "black",
        face = "bold"
      ),
      axis.title.x = element_text(
        margin = margin(t = 7)
      ),
      axis.title.y = element_text(
        margin = margin(r = 7)
      ),

      # Major and minor ticks point inward. Negative lengths move ticks into
      # the plotting panel rather than outward toward the labels.
      axis.ticks = element_line(
        color = "black",
        linewidth = 0.5
      ),
      axis.minor.ticks = element_line(
        color = "black",
        linewidth = 0.4
      ),
      axis.ticks.length = grid::unit(-0.12, "cm"),
      axis.minor.ticks.length = grid::unit(-0.07, "cm"),

      # -----------------------------------------------------------------------
      # Plot title, subtitle, facets, and caption
      # -----------------------------------------------------------------------
      plot.title = element_text(
        size = 14,
        face = "bold",
        color = "black",
        hjust = 0.5,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        size = 10,
        face = "plain",
        color = "black",
        hjust = 0.5,
        margin = margin(b = 7)
      ),
      strip.background = element_blank(),
      strip.text = element_text(
        size = 11,
        face = "bold",
        color = "black"
      ),
      plot.caption = element_text(
        size = 9,
        color = "black",
        hjust = 0,
        margin = margin(t = 6)
      ),

      # -----------------------------------------------------------------------
      # Legend inside the lower-right corner
      # -----------------------------------------------------------------------
      legend.position = "inside",
      legend.position.inside = LegendPosition,
      legend.justification = LegendJustification,
      legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(
        size = 10,
        color = "black"
      ),
      legend.key = element_rect(
        fill = "white",
        color = NA
      ),
      legend.background = element_rect(
        fill = scales::alpha("white", 0.90),
        color = NA
      ),
      legend.box.background = element_blank(),
      legend.margin = margin(4, 4, 4, 4),

      plot.title.position = "panel",
      plot.caption.position = "plot",
      plot.margin = margin(8, 8, 8, 8)
    )
}


# ggplot2 separates the styling of minor ticks from enabling them. Add this
# helper after the continuous scales in a plotting function:
#
#   + Theme_AS()
#   + PublicationGuides()
#
# Continuous scales normally place one minor break halfway between adjacent
# major breaks. Set minor_breaks explicitly in scale_x/y_continuous() whenever
# a plot needs a different spacing.

PublicationGuides <- function() {
  guides(
    x = guide_axis(minor.ticks = TRUE),
    y = guide_axis(minor.ticks = TRUE)
  )
}


# =============================================================================
# Common plot-specific overrides
# =============================================================================

# Move the legend to the upper-right:
# + theme(
#     legend.position.inside = c(0.97, 0.97),
#     legend.justification = c(1, 1)
#   )

# Put the legend outside below the plot:
# + theme(
#     legend.position = "bottom",
#     legend.direction = "horizontal"
#   )

# Hide the legend:
# + theme(legend.position = "none")

# Left-align the title for a particular plot:
# + theme(plot.title = element_text(hjust = 0))

# Remove the subtitle without changing the plotting function:
# + theme(plot.subtitle = element_blank())