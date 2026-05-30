# =============================================================================
# PlotVoltageTime.R
# Goal: For each file (cell), plot Voltage vs Time (hours)
#       One line per cycle, colored by cycle number (viridis)
#       All step types included (charge, discharge, rest)
#       Saved to: outputs/Individual/<CellName>/voltage_vs_time.png
# Requires: CyclingData data frame produced by ExtractCycleData()
# =============================================================================

PlotVoltageTime <- function(CyclingData,
                            OutputDir  = "outputs/Individual",
                            PaddingPct = 0.03,
                            PlotWidth  = 8,
                            PlotHeight = 5,
                            DPI        = 300) {
  
  # --- Validate required columns ----------------------------------------
  RequiredCols <- c("File", "Cycle", "Voltage", "TestTime", "StepType")
  MissingCols  <- setdiff(RequiredCols, colnames(CyclingData))
  if (length(MissingCols) > 0) {
    stop("CyclingData is missing required columns: ", paste(MissingCols, collapse = ", "))
  }
  
  # --- Convert time to hours and drop incomplete cycles -----------------
  PlotData <- CyclingData |>
    filter(!is.na(Cycle), !is.na(Voltage), !is.na(TestTime)) |>
    group_by(File) |>
    filter(Cycle != max(Cycle)) |>
    ungroup() |>
    mutate(TestTime_h = TestTime / 3600)
  
  Files <- unique(PlotData$File)
  cat("=== PlotVoltageTime ===\n")
  cat("Files to plot:", length(Files), "\n\n")
  
  # --- Loop over each file (cell) ---------------------------------------
  walk(Files, function(fname) {
    
    FileData <- PlotData |> filter(File == fname)
    NCycles  <- n_distinct(FileData$Cycle)
    CellName <- tools::file_path_sans_ext(fname)
    
    cat(sprintf("  Plotting: %s\n", fname))
    cat(sprintf("    Cycles detected : %d\n", NCycles))
    
    # --- Axis limits ----------------------------------------------------
    TimeRange <- range(FileData$TestTime_h, na.rm = TRUE)
    VoltRange <- range(FileData$Voltage,    na.rm = TRUE)
    
    TimePad <- diff(TimeRange) * PaddingPct
    VoltPad <- diff(VoltRange) * PaddingPct
    
    XLim <- c(TimeRange[1] - TimePad, TimeRange[2] + TimePad)
    YLim <- c(VoltRange[1] - VoltPad, VoltRange[2] + VoltPad)
    
    cat(sprintf("    Time range      : [%.2f, %.2f] hours\n", XLim[1], XLim[2]))
    cat(sprintf("    Voltage range   : [%.3f, %.3f] V\n\n",   YLim[1], YLim[2]))
    
    # --- Build plot -----------------------------------------------------
    p <- ggplot(FileData, aes(
      x     = TestTime_h,
      y     = Voltage,
      group = factor(Cycle),
      color = Cycle
    )) +
      geom_line(linewidth = 0.5, alpha = 0.85) +
      scale_color_viridis_c(
        name   = "Cycle",
        option = "viridis",
        breaks = scales::pretty_breaks(n = max(min(NCycles, 6), 2))
      ) +
      scale_x_continuous(
        limits = XLim,
        breaks = scales::pretty_breaks(n = 6),
        labels = label_number(accuracy = 0.1),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        limits       = YLim,
        breaks       = scales::pretty_breaks(n = 6),
        #minor_breaks = if (diff(VoltRange) > 0) scales::pretty_breaks(n = 12) else NULL,
        labels       = label_number(accuracy = 0.1),
        expand       = c(0, 0),
        guide        = guide_axis(minor.ticks = TRUE)
      ) +
      labs(
        title    = CellName,
        subtitle = sprintf("%d cycles", NCycles),
        x        = "Time (hours)",
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
    SavePath <- file.path(SaveDir, "voltage_vs_time.png")
    
    dir.create(SaveDir, recursive = TRUE, showWarnings = FALSE)
    
    ggsave(SavePath, plot = p, width = PlotWidth, height = PlotHeight, dpi = DPI)
    cat(sprintf("    Saved: %s\n\n", SavePath))
  })
  
  cat(sprintf("=== Done — %d plot(s) saved to: %s ===\n", length(Files), OutputDir))
}


# =============================================================================
# Example usage:
#
#   source("LoadLibs.R")
#   source("ActiveMass.R")
#   source("GetFiles.R")
#   source("ExtractCyclingDataArbin.R")
#   source("PlotVoltageTime.R")
#
#   FileList    <- GetFiles("data/raw")
#   CyclingData <- ExtractCycleData(FileList)
#   PlotVoltageTime(CyclingData)
#
# =============================================================================