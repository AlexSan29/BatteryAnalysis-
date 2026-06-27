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
    geom_point(size = 1.5) +
    geom_line(linewidth = .8) +
    scale_color_manual(
      values = c(
        "LMRO_Cycle_1" = "#f7572a",
        "LMRO_Cycle_2" = "#48d80e",
        "LMRO_Cycle_3" = "#f53520",
        "LMRO_Cycle_4" = "#c9b3e4",
        "LMRO_Cycle_5" = "#7a0eff",
        "LMRO_Cycle_6" = "#7a0eff",
        "LMRO_Cycle_7" = "#f7572a",
        "LMRO_Cycle_8" = "#7a0eff",
        "LMRO_Cycle_9" = "#f7572a",
        "LMRO_Cycle_11" = "#f7572a",
        "LMRO_Cycle_12" = "#7a0eff",
        "LMRO_Cycle_13" = "#f7572a",
        "DOE_Cycle_2_1" = "#00BFC4",
        "DOE_Cycle_2_2" = "#C77CFF",
        "DOE_Cycle_2_3" = "#FF61CC",
        "DOE_Cycle_2_4" = "#476a51",
        "DOE_Cycle_2_5" = "#619CFF"

      ),
      labels = c(
        "LMRO_Cycle_1" = "Cell 1\n N:P 0.94",
        "LMRO_Cycle_2" = "Cell 2\n N:P 0.87",
        "LMRO_Cycle_3" = "Cell 3 Gr || Ru",
        "LMRO_Cycle_4" = "Cell 1\nConditioning",
        "LMRO_Cycle_5" = "Cell 2\nConditioning",
        "LMRO_Cycle_6" = "Cell 3\nConditioning",
        "LMRO_Cycle_7" = "Cell 4\nFull Window ",
        "LMRO_Cycle_8" = "Cell 3 \n N:P 1.5",
        "LMRO_Cycle_9" = "Cell 9\nLMRO Full Window",
        "LMRO_Cycle_11" = "Cell 11\nNMC811 Full Window",
        "LMRO_Cycle_12" = "Cell 12\nNMC811 Conditioning",
        "LMRO_Cycle_13" = "Cell 13\nNMC811 Conditioning",
        "DOE_Cycle_2_1" = "111 -> SEM/EDS ",
        "DOE_Cycle_2_2" = "111 Idle at 100C",
        "DOE_Cycle_2_3" = "811 -> SEM/EDS ",
        "DOE_Cycle_2_4" = "111 Rate Capability",
        "DOE_Cycle_2_5" = "111 + VC -> SEM/EDS"

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
      title = "Discharge Capacity per Cycle",
      subtitle = "Cells with Li Metal anodes",
      x = "Cycle",
      y = expression("Specific Discharge Capacity (mAh g"^-1*")")
    ) + 
    Theme_AS()

  ggsave(
    file.path("outputs", "DischargeCapacityPerCycle.png"),
    DischargePlot,
    width  = 8,
    height = 5.5,
    dpi    = 300
  )

  cat("Plotting discharge capacity per cycle ... Done\n")

  DischargePlot
}