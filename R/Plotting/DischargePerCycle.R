DischargePerCyclePlot <- function(PerCycleData) {

  XMax         <- max(PerCycleData$Cycle, na.rm = TRUE)
  XMajorBreaks <- seq(0, XMax, by = 5)

  YMaxRaw      <- max(PerCycleData$MaxSpecificDischargeCapacity, na.rm = TRUE)
  YMax         <- ceiling(YMaxRaw / 50) * 50
  YMajorBreaks <- seq(0, YMax, by = 50)
  YMinorBreaks <- seq(0, YMax, by = 25)

  DischargePlot <- PerCycleData |>
    group_by(File, Cycle) |>
    ggplot(aes(Cycle, MaxSpecificDischargeCapacity, color = File)) +
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
      y = expression("Specific Discharge Capacity (mAh g"^-1*")")
    ) +
    Theme_AS()

  ggsave(
    file.path("outputs", "DischargeCapacityPerCycle.png"),
    DischargePlot,
    width  = 8.5,
    height = 4.5,
    dpi    = 300
  )

  cat("Plotting discharge capacity per cycle ... Done\n")

  DischargePlot
}