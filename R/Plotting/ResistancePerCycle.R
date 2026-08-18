ResistancePerCyclePlot <- function(PerCycleData) {

  XMax         <- max(PerCycleData$Cycle, na.rm = TRUE)
  XMajorBreaks <- seq(0, XMax, by = 5)

  YMaxRaw      <- max(PerCycleData$Resistance, na.rm = TRUE)
  YMax         <- 100
  
  YMajorBreaks <- seq(0, YMax, by = 20)
  YMinorBreaks <- seq(0, YMax, by = 10)

  ResistancePlot <- PerCycleData |>
    group_by(File, Cycle) |>
    ggplot(aes(Cycle, Resistance, color = File)) +
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
      breaks       = YMajorBreaks,
      minor_breaks = YMinorBreaks,
      limits       = c(0, YMax),
      labels       = label_number(accuracy = 1),
      guide        = guide_axis(minor.ticks = TRUE)
    ) +
    labs(
      title = "Resistance per Cycle",
      subtitle = "Cells with Li Metal anodes",
      x = "Cycle",
      y = expression("Resistance (Ω)")
    ) + 
    Theme_AS()

  ggsave(
    file.path("outputs", "ResistancePerCycle.png"),
    ResistancePlot,
    width  = 8,
    height = 5.5,
    dpi    = 300
  )

  cat("Plotting resistance per cycle ... Done\n")

  ResistancePlot
}