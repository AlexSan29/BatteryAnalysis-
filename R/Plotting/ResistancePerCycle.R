ResistancePerCyclePlot <- function(PerCycleData) {

  XMax         <- max(PerCycleData$Cycle, na.rm = TRUE)
  XMajorBreaks <- seq(0, XMax, by = 5)

  YMaxRaw      <- max(PerCycleData$Resistance, na.rm = TRUE)
  YMax         <- ceiling(YMaxRaw / 10) * 10
  YMajorBreaks <- seq(0, YMax, by = 10)
  YMinorBreaks <- seq(0, YMax, by = 5)

  ResistancePlot <- PerCycleData |>
    group_by(File, Cycle) |>
    ggplot(aes(Cycle, Resistance, color = File)) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = c(
        "LMRO_Cycle_1" = "#F8766D",
        "LMRO_Cycle_2" = "#7CAE00",
        "LMRO_Cycle_4" = "#00BFC4",
        "LMRO_Cycle_5" = "#C77CFF"
      ),
      labels = c(
        "LMRO_Cycle_1" = "Cell 1 Gr || Ru",
        "LMRO_Cycle_2" = "Cell 2 Gr || Ru",
        "LMRO_Cycle_4" = "Cell 4 Li || Ru",
        "LMRO_Cycle_5" = "Cell 5 Li || Ru"
      )
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
      x = "Cycle",
      y = "Resistance (\u03a9)"
    ) +
    Theme_AS()

  ggsave(
    file.path("outputs", "ResistancePerCycle.png"),
    ResistancePlot,
    width  = 8.5,
    height = 4.5,
    dpi    = 300
  )

  cat("Plotting resistance per cycle... Done\n")

  ResistancePlot
}