# =============================================================================
# PlotChargeCurves.R
# Goal: For each file (cell), plot Voltage vs Specific Charge Capacity
#       One line per cycle, colored by cycle number (viridis)
#       Saved to: outputs/Individual/<CellName>/charge_curves.png
# Requires: CyclingData data frame produced by ExtractCycleData()
# =============================================================================

PlotChargeCurves <- function(CyclingData,
                             OutputDir  = "outputs/Individual",
                             PaddingPct = 0.03,
                             PlotWidth  = 8,
                             PlotHeight = 5,
                             DPI        = 300) {
  
  # --- Validate required columns ----------------------------------------
  RequiredCols <- c("File", "Cycle", "Voltage", "SpecificChargeCapacity", "StepType")
  MissingCols  <- setdiff(RequiredCols, colnames(CyclingData))
  if (length(MissingCols) > 0) {
    stop("CyclingData is missing required columns: ", paste(MissingCols, collapse = ", "))
  }
  
  # --- Filter to charge rows only ---------------------------------------
  ChargeData <- CyclingData |>
    filter(StepType == "Charge", !is.na(Cycle), !is.na(Voltage), !is.na(SpecificChargeCapacity))
  
  Files <- unique(ChargeData$File)
  cat("=== PlotChargeCurves ===\n")
  cat("Files to plot:", length(Files), "\n\n")
  
  # --- Loop over each file (cell) ---------------------------------------
  walk(Files, function(fname) {
    
    FileData <- ChargeData |> filter(File == fname)
    NCycles  <- n_distinct(FileData$Cycle)
    CellName <- tools::file_path_sans_ext(fname)
    
    cat(sprintf("  Plotting: %s\n", fname))
    cat(sprintf("    Cycles detected : %d\n", NCycles))
    
    # --- Axis limits ----------------------------------------------------
    # X: floor at 0 so charge curves land on axis, pad top only
    # Y: symmetric padding top and bottom
    CapRange  <- range(FileData$SpecificChargeCapacity, na.rm = TRUE)
    VoltRange <- range(FileData$Voltage, na.rm = TRUE)
    
    VoltPad <- diff(VoltRange) * PaddingPct
    
    XLim <- c(0, CapRange[2] * (1 + PaddingPct))
    YLim <- c(VoltRange[1] - VoltPad, VoltRange[2] + VoltPad)
    
    cat(sprintf("    Capacity range  : [%.2f, %.2f] mAh/g\n", XLim[1], XLim[2]))
    cat(sprintf("    Voltage range   : [%.3f, %.3f] V\n\n",   YLim[1], YLim[2]))
    
    # --- Build plot -----------------------------------------------------
    p <- ggplot(FileData, aes(
      x     = SpecificChargeCapacity,
      y     = Voltage,
      group = factor(Cycle),
      color = Cycle
    )) +
      geom_line(linewidth = 0.6, alpha = 0.85) +
      scale_color_viridis_c(
        name   = "Cycle",
        option = "viridis",
        breaks = scales::pretty_breaks(n = min(NCycles, 6))
      ) +
      scale_x_continuous(
        limits = XLim,
        breaks = scales::pretty_breaks(n = 6),
        labels = label_number(accuracy = 1),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        limits       = YLim,
        breaks       = scales::pretty_breaks(n = 6),
        #minor_breaks = scales::pretty_breaks(n = 12),
        labels       = label_number(accuracy = 0.1),
        expand       = c(0, 0),
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
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 10)
      )
    
    # --- Save output ----------------------------------------------------
    SaveDir  <- file.path(OutputDir, CellName)
    SavePath <- file.path(SaveDir, "charge_curves.png")
    
    dir.create(SaveDir, recursive = TRUE, showWarnings = FALSE)
    
    ggsave(SavePath, plot = p, width = PlotWidth, height = PlotHeight, dpi = DPI)
    cat(sprintf("    Saved: %s\n\n", SavePath))
  })
  
  cat(sprintf("=== Done — %d plot(s) saved to: %s ===\n", length(Files), OutputDir))
}

