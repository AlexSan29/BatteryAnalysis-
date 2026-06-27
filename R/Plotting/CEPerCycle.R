CEPerCyclePlot <- function(PerCycleStats) {

  XMax         <- max(PerCycleStats$Cycle, na.rm = TRUE)
  XMajorBreaks <- seq(0, XMax, by = 5)
  YLowerLimit   <- 50
  YUpperLimit   <- 110

  CoulombicPlot <- PerCycleStats |>
    group_by(File, Cycle) |>
    ggplot(aes(Cycle, CoulombicEfficiency, color = File)) +
    geom_point(size = 1.5) +
    geom_line(linewidth = .8) +
    scale_color_manual(
      values = PlotColors,
      labels = PlotLabels
    ) +
    scale_x_continuous(
      breaks = XMajorBreaks,
      limits = c(1, XMax),
      labels = label_number(accuracy = 1)
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      minor_breaks = seq(YLowerLimit, YUpperLimit, by = 2.5),
      labels = label_number(accuracy = 1),
      expand = c(0.02, 0),
      guide  = guide_axis(minor.ticks = TRUE)
    ) +
    coord_cartesian(ylim = c(YLowerLimit, YUpperLimit)) +
    labs(
      title = "Coulombic Efficiency per Cycle",
      subtitle = "Cells with Li Metal anodes",
      x = "Cycle",
      y = "Coulombic Efficiency (%)"
    ) +
    Theme_AS()

  ggsave(
    file.path("outputs", "CEPerCycle.png"),
    CoulombicPlot,
    width  = 8,
    height = 5.5,
    dpi    = 300
  )

  cat("Plotting coulombic efficiency per cycle ... Done\n")

  CoulombicPlot
}