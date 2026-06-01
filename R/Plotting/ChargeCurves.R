ChargeCurves <- function(CyclingData,
                             OutputDir  = "outputs/Individual", # nolint: indentation_linter.
                             PaddingPct = 0.02,
                             PlotWidth  = 8,
                             PlotHeight = 5,
                             DPI        = 300) {

  RequiredCols <- c("File", "Cycle", "Voltage", "SpecificChargeCapacity", "StepType")
  MissingCols  <- setdiff(RequiredCols, colnames(CyclingData))
  if (length(MissingCols) > 0) {
    stop("CyclingData is missing required columns: ", paste(MissingCols, collapse = ", "))
  }

  ChargeData <- CyclingData |>
    filter(StepType == "Charge", !is.na(Cycle), !is.na(Voltage), !is.na(SpecificChargeCapacity))

  Files <- unique(ChargeData$File)

  walk(Files, function(Fname) {

    FileData <- ChargeData |> filter(File == Fname)
    NCycles  <- n_distinct(FileData$Cycle)
    CellName <- tools::file_path_sans_ext(Fname)

    CapRange  <- range(FileData$SpecificChargeCapacity, na.rm = TRUE)
    VoltRange <- range(FileData$Voltage[FileData$Voltage > 1.29], na.rm = TRUE)
    VoltPad   <- diff(VoltRange) * PaddingPct

    XLim <- c(0, CapRange[2] * (1 + PaddingPct))
    YLim <- c(1.3, 4.6)

    P <- ggplot(FileData, aes(
      x     = SpecificChargeCapacity,
      y     = Voltage,
      group = factor(Cycle),
      color = Cycle
    )) +
      geom_line(linewidth = 0.6, alpha = 0.85) +
      scale_color_viridis_c(
        name   = "Cycle",
        option = "viridis",
        breaks = scales::pretty_breaks(n = max(min(NCycles, 6), 2))
      ) +
      scale_x_continuous(
        limits = XLim,
        breaks = scales::pretty_breaks(n = 6),
        labels = label_number(accuracy = 1),
        expand = c(0.02, 0)
      ) +
      scale_y_continuous(
        limits = YLim,
        breaks = seq(
          YLim[1],
          YLim[2],
          by = 0.3
        ),
        labels       = label_number(accuracy = 0.1),
        expand       = c(0.02, 0),
        guide        = guide_axis(minor.ticks = TRUE)
      ) +
      labs(
        title    = CellName,
        subtitle = sprintf("%d cycles", NCycles),
        x        = "Specific Charge Capacity (mAh/g)",
        y        = "Voltage (V)"
      ) +
      Theme_AS() +
      theme(
        legend.position  = "right",
        legend.direction = "vertical",
        legend.title     = element_text(size = 12),
        legend.text      = element_text(size = 10)
      )

    SaveDir  <- file.path(OutputDir, CellName)
    SavePath <- file.path(SaveDir, "ChargeCurves.png")
    dir.create(SaveDir, recursive = TRUE, showWarnings = FALSE)
    ggsave(SavePath, plot = P, width = PlotWidth, height = PlotHeight, dpi = DPI)
  })

  cat(sprintf("Plotting charge curves... Done (%d files)\n", length(Files)))
}