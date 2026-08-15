# =============================================================================
# DqDv.R
# Calculates and plots differential capacity (dQ/dV).
#
# Improvements in this version:
#   1. Uses a regular voltage grid instead of raw diff(Q) / diff(V).
#   2. Uses a Savitzky-Golay derivative to reduce numerical noise.
#   3. Trims filter-edge artifacts.
#   4. Supports selection of specific cycles.
#   5. Excludes DCIR blocks when KeepChargeCurve / KeepDischargeCurve exist.
#   6. Exports one plotting-ready Excel workbook per input file.
#
# Required packages already used elsewhere in the pipeline:
#   dplyr, tidyr, purrr, ggplot2, signal, scales
#
# Additional package for Excel export:
#   writexl
# =============================================================================

DqDv <- function(CyclingData,
                 OutputDir       = "outputs/Individual",
                 Cycles          = NULL,
                 VoltageStep     = 0.005,
                 SgWindow        = 21,
                 SgDegree        = 3,
                 TrimEdges       = TRUE,
                 MaxAbsDqDv      = 500,
                 ExcludeLastCycle = FALSE,
                 ExportXlsx      = TRUE,
                 PlotWidth       = 8,
                 PlotHeight      = 5,
                 DPI             = 300) {

  RequiredCols <- c(
    "File", "Cycle", "Voltage", "SpecificChargeCapacity",
    "SpecificDischargeCapacity", "StepType"
  )
  MissingCols <- setdiff(RequiredCols, colnames(CyclingData))

  if (length(MissingCols) > 0) {
    stop(
      "CyclingData is missing required columns: ",
      paste(MissingCols, collapse = ", ")
    )
  }

  if (!is.null(Cycles) && (!is.numeric(Cycles) || length(Cycles) == 0)) {
    stop("Cycles must be NULL or a numeric vector such as c(1, 5, 10, 25).")
  }

  if (!is.numeric(VoltageStep) || length(VoltageStep) != 1 ||
      is.na(VoltageStep) || VoltageStep <= 0) {
    stop("VoltageStep must be one positive number in volts.")
  }

  if (SgWindow %% 2 == 0) {
    stop("SgWindow must be an odd integer, such as 15, 21, 31, or 41.")
  }

  if (SgDegree >= SgWindow) {
    stop("SgDegree must be smaller than SgWindow.")
  }

  if (ExportXlsx && !requireNamespace("writexl", quietly = TRUE)) {
    stop(
      "Excel export requires the writexl package. Install it once with: ",
      "install.packages('writexl')"
    )
  }

  # ---------------------------------------------------------------------------
  # Calculate dQ/dV for one charge or discharge curve.
  # ---------------------------------------------------------------------------
  ComputeDqDv <- function(Voltage, Capacity) {

    InputData <- tibble(
      Voltage = as.numeric(Voltage),
      Capacity = as.numeric(Capacity)
    ) |>
      filter(is.finite(Voltage), is.finite(Capacity))

    if (nrow(InputData) < 5) {
      return(tibble(Voltage = numeric(), DqDv = numeric()))
    }

    # Collapse repeated/nearly repeated voltages before interpolation. Taking
    # the median capacity within each voltage bin is resistant to isolated
    # logging noise and prevents division by an extremely small delta-V.
    BinnedData <- InputData |>
      mutate(VoltageBin = round(Voltage / VoltageStep) * VoltageStep) |>
      group_by(VoltageBin) |>
      summarise(Capacity = median(Capacity, na.rm = TRUE), .groups = "drop") |>
      arrange(VoltageBin)

    if (nrow(BinnedData) < 5) {
      return(tibble(Voltage = numeric(), DqDv = numeric()))
    }

    GridStart <- ceiling(min(BinnedData$VoltageBin) / VoltageStep) * VoltageStep
    GridEnd   <- floor(max(BinnedData$VoltageBin) / VoltageStep) * VoltageStep

    if (!is.finite(GridStart) || !is.finite(GridEnd) || GridEnd <= GridStart) {
      return(tibble(Voltage = numeric(), DqDv = numeric()))
    }

    VoltageGrid <- seq(GridStart, GridEnd, by = VoltageStep)

    CapacityGrid <- approx(
      x = BinnedData$VoltageBin,
      y = BinnedData$Capacity,
      xout = VoltageGrid,
      method = "linear",
      ties = "ordered",
      rule = 1
    )$y

    ValidGrid <- is.finite(VoltageGrid) & is.finite(CapacityGrid)
    VoltageGrid  <- VoltageGrid[ValidGrid]
    CapacityGrid <- CapacityGrid[ValidGrid]

    if (length(VoltageGrid) < 5) {
      return(tibble(Voltage = numeric(), DqDv = numeric()))
    }

    # Make the requested window fit shorter curves while keeping it odd.
    LocalWindow <- min(SgWindow, length(VoltageGrid))
    if (LocalWindow %% 2 == 0) LocalWindow <- LocalWindow - 1

    MinimumWindow <- SgDegree + 2
    if (MinimumWindow %% 2 == 0) MinimumWindow <- MinimumWindow + 1

    if (LocalWindow < MinimumWindow) {
      return(tibble(Voltage = numeric(), DqDv = numeric()))
    }

    # m = 1 calculates the first derivative directly. ts supplies the voltage
    # spacing, so the result has units of mAh/g/V.
    Derivative <- signal::sgolayfilt(
      CapacityGrid,
      p = SgDegree,
      n = LocalWindow,
      m = 1,
      ts = VoltageStep
    )

    Result <- tibble(
      Voltage = VoltageGrid,
      DqDv = as.numeric(Derivative)
    )

    # Savitzky-Golay estimates are least reliable within half a window of each
    # endpoint. Removing those regions prevents artificial endpoint spikes.
    if (TrimEdges) {
      EdgePoints <- floor(LocalWindow / 2)
      if (nrow(Result) > 2 * EdgePoints) {
        KeepRows <- seq.int(
          EdgePoints + 1,
          nrow(Result) - EdgePoints
        )
        Result <- Result[KeepRows, , drop = FALSE]
      }
    }

    if (!is.null(MaxAbsDqDv) && is.finite(MaxAbsDqDv)) {
      Result <- Result |>
        filter(abs(DqDv) <= MaxAbsDqDv)
    }

    Result |>
      filter(is.finite(Voltage), is.finite(DqDv))
  }

  # ---------------------------------------------------------------------------
  # Select rows eligible for differential-capacity analysis.
  # ---------------------------------------------------------------------------
  PlotData <- CyclingData |>
    filter(
      StepType %in% c("Charge", "Discharge"),
      !is.na(File),
      !is.na(Cycle),
      !is.na(Voltage)
    )

  # Use the DCIR filtering flags when the updated cycling scripts created them.
  # The fallback keeps this DqDv script compatible with older extracted data.
  if (all(c("KeepChargeCurve", "KeepDischargeCurve") %in%
          colnames(PlotData))) {
    PlotData <- PlotData |>
      filter(
        (StepType == "Charge" & KeepChargeCurve) |
          (StepType == "Discharge" & KeepDischargeCurve)
      )
  }

  if (!is.null(Cycles)) {
    PlotData <- PlotData |>
      filter(Cycle %in% Cycles)
  }

  if (ExcludeLastCycle) {
    PlotData <- PlotData |>
      group_by(File) |>
      filter(Cycle != max(Cycle, na.rm = TRUE)) |>
      ungroup()
  }

  Files <- unique(PlotData$File)

  walk(Files, function(Fname) {

    FileData <- PlotData |>
      filter(File == Fname)

    SelectedCycles <- sort(unique(FileData$Cycle))
    NCycles <- length(SelectedCycles)
    CellName <- tools::file_path_sans_ext(Fname)

    DqDvData <- FileData |>
      group_by(File, Cycle, StepType) |>
      group_modify(~ {
        Dat <- .x

        Capacity <- if (.y$StepType == "Charge") {
          Dat$SpecificChargeCapacity
        } else {
          Dat$SpecificDischargeCapacity
        }

        ComputeDqDv(Dat$Voltage, Capacity)
      }) |>
      ungroup() |>
      arrange(Cycle, StepType, Voltage)

    if (nrow(DqDvData) == 0) {
      cat(sprintf("  ! %s — no valid dQ/dV data, skipping\n", CellName))
      return(invisible(NULL))
    }

    # -------------------------------------------------------------------------
    # Plot
    # -------------------------------------------------------------------------
    P <- ggplot(DqDvData, aes(
      x = Voltage,
      y = DqDv,
      group = interaction(Cycle, StepType),
      color = Cycle
    )) +
      geom_line(linewidth = 0.6, alpha = 0.90) +
      geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
      CycleColorScale(NCycles) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 6),
        labels = label_number(accuracy = 0.1),
        expand = c(0.02, 0)
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 6),
        labels = label_number(accuracy = 1),
        expand = c(0.02, 0)
      ) +
      labs(
        title = GetPlotTitle(CellName),
        subtitle = paste0(
          NCycles,
          ifelse(NCycles == 1, " selected cycle", " selected cycles"),
          ": ",
          paste(SelectedCycles, collapse = ", ")
        ),
        x = "Voltage (V)",
        y = "dQ/dV (mAh/g/V)"
      ) +
      Theme_AS() +
      IndividualPlotTheme()

    SaveDir  <- file.path(OutputDir, CellName)
    SavePath <- file.path(SaveDir, "DqDv.png")
    dir.create(SaveDir, recursive = TRUE, showWarnings = FALSE)

    ggsave(
      SavePath,
      plot = P,
      width = PlotWidth,
      height = PlotHeight,
      dpi = DPI
    )

    # -------------------------------------------------------------------------
    # Excel export: one workbook for each file/cell.
    #
    # Long_Data is analysis-friendly. Plot_Ready has one voltage column and one
    # dQ/dV column per cycle/direction, making Excel XY-scatter plotting fast.
    # -------------------------------------------------------------------------
    if (ExportXlsx) {

      LongData <- DqDvData |>
        transmute(
          Cycle,
          Direction = StepType,
          `Voltage (V)` = Voltage,
          `dQ/dV (mAh/g/V)` = DqDv
        )

      PlotReady <- DqDvData |>
        mutate(
          Series = paste0(StepType, "_Cycle_", Cycle)
        ) |>
        select(Voltage, Series, DqDv) |>
        tidyr::pivot_wider(
          names_from = Series,
          values_from = DqDv,
          values_fn = mean
        ) |>
        arrange(Voltage) |>
        rename(`Voltage (V)` = Voltage)

      Settings <- tibble(
        Setting = c(
          "Selected cycles",
          "Voltage grid spacing (V)",
          "Savitzky-Golay window",
          "Savitzky-Golay degree",
          "Edge trimming",
          "Maximum absolute dQ/dV",
          "Last cycle excluded"
        ),
        Value = c(
          paste(SelectedCycles, collapse = ", "),
          as.character(VoltageStep),
          as.character(SgWindow),
          as.character(SgDegree),
          as.character(TrimEdges),
          ifelse(is.null(MaxAbsDqDv), "None", as.character(MaxAbsDqDv)),
          as.character(ExcludeLastCycle)
        )
      )

      ExcelPath <- file.path(SaveDir, "DqDv_Data.xlsx")

      writexl::write_xlsx(
        list(
          Plot_Ready = PlotReady,
          Long_Data = LongData,
          Settings = Settings
        ),
        path = ExcelPath,
        col_names = TRUE,
        format_headers = TRUE
      )
    }
  })

  cat(sprintf(
    "Plotting dQ/dV curves... Done (%d files)\n",
    length(Files)
  ))
}


# =============================================================================
# Examples
# =============================================================================

# Plot and export every available cycle:
# DqDv(CyclingData)

# Plot and export selected cycles:
# DqDv(CyclingData, Cycles = c(1, 2, 5, 10, 25))

# Plot every fifth cycle:
# DqDv(CyclingData, Cycles = seq(5, 50, by = 5))

# Stronger smoothing: increase the window while keeping it odd:
# DqDv(CyclingData, Cycles = c(1, 5, 10, 25), SgWindow = 31)

# Finer voltage resolution with somewhat less smoothing:
# DqDv(CyclingData, VoltageStep = 0.002, SgWindow = 21)

# Create plots only, without Excel files:
# DqDv(CyclingData, ExportXlsx = FALSE)