DischargeCurves <- function(CyclingData,
                            OutputDir  = "outputs/Individual",
                            PaddingPct = 0.02,
                            PlotWidth  = 8,
                            PlotHeight = 5.5,
                            DPI        = 300) {

  RequiredCols <- c(
    "File", "Cycle", "Voltage", "SpecificDischargeCapacity",
    "StepType", "CurveBlock", "KeepDischargeCurve"
  )
  MissingCols <- setdiff(RequiredCols, colnames(CyclingData))
  if (length(MissingCols) > 0) {
    stop(
      "CyclingData is missing required columns: ",
      paste(MissingCols, collapse = ", ")
    )
  }

  DischargeData <- CyclingData |>
    filter(
      StepType == "Discharge",
      KeepDischargeCurve,
      !is.na(Cycle),
      !is.na(Voltage),
      !is.na(SpecificDischargeCapacity)
    )

  Files <- unique(DischargeData$File)

  walk(Files, function(Fname) {

    FileData <- DischargeData |> filter(File == Fname)
    NCycles  <- n_distinct(FileData$Cycle)
    CellName <- tools::file_path_sans_ext(Fname)

    CapRange <- range(FileData$SpecificDischargeCapacity, na.rm = TRUE)
    XLim     <- c(0, CapRange[2] * (1 + PaddingPct))

    P <- ggplot(FileData, aes(
      x = SpecificDischargeCapacity,
      y = Voltage,
      group = interaction(Cycle, CurveBlock),
      color = Cycle
    )) +
      geom_line(linewidth = 0.6, alpha = 0.85) +
      CycleColorScale(NCycles) +
      scale_x_continuous(
        limits = XLim,
        breaks = scales::pretty_breaks(n = 6),
        labels = label_number(accuracy = 1),
        expand = c(0.02, 0)
      ) +
      scale_y_continuous(
        limits = VoltageYLim,
        breaks = seq(VoltageYLim[1], VoltageYLim[2], by = VoltageYBreak),
        labels = label_number(accuracy = 0.1),
        expand = c(0.02, 0),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      labs(
        title = GetPlotTitle(CellName),
        subtitle = sprintf("%d cycles", NCycles),
        x = "Specific Discharge Capacity (mAh/g)",
        y = "Voltage (V)"
      ) +
      Theme_AS() +
      IndividualPlotTheme()

    SaveDir  <- file.path(OutputDir, CellName)
    SavePath <- file.path(SaveDir, "DischargeCurves.png")
    dir.create(SaveDir, recursive = TRUE, showWarnings = FALSE)
    ggsave(
      SavePath, plot = P,
      width = PlotWidth, height = PlotHeight, dpi = DPI
    )
  })

  cat(sprintf("Plotting discharge curves... Done (%d files)\n", length(Files)))
}