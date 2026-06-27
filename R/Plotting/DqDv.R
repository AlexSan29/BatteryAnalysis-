DqDv <- function(CyclingData,
                 OutputDir  = "outputs/Individual",
                 SgWindow   = 21,
                 SgDegree   = 3,
                 PlotWidth  = 8,
                 PlotHeight = 5,
                 DPI        = 300) {

  RequiredCols <- c("File", "Cycle", "Voltage", "SpecificChargeCapacity",
                    "SpecificDischargeCapacity", "StepType")
  MissingCols <- setdiff(RequiredCols, colnames(CyclingData))
  if (length(MissingCols) > 0) {
    stop("CyclingData is missing required columns: ", paste(MissingCols, collapse = ", "))
  }

  ComputeDqDv <- function(Voltage, Capacity) {
    dQ    <- diff(Capacity)
    dV    <- diff(Voltage)
    V_mid <- (Voltage[-length(Voltage)] + Voltage[-1]) / 2

    Valid <- abs(dV) > 1e-4
    dQdV  <- ifelse(Valid, dQ / dV, NA_real_)
    dQdV  <- ifelse(abs(dQdV) > 500, NA_real_, dQdV)

    NonNA <- which(!is.na(dQdV) & is.finite(dQdV))
    if (length(NonNA) >= SgWindow) {
      dQdV[NonNA] <- signal::sgolayfilt(dQdV[NonNA], p = SgDegree, n = SgWindow)
    }

    tibble(Voltage = V_mid, DqDv = dQdV)
  }

  PlotData <- CyclingData |>
    filter(
      StepType %in% c("Charge", "Discharge"),
      !is.na(Cycle), !is.na(Voltage)
    ) |>
    group_by(File) |>
    filter(Cycle != max(Cycle)) |>
    ungroup()

  Files <- unique(PlotData$File)

  walk(Files, function(Fname) {

    FileData <- PlotData |> filter(File == Fname)
    NCycles  <- n_distinct(FileData$Cycle)
    CellName <- tools::file_path_sans_ext(Fname)

    DqDvData <- FileData |>
      group_by(File, Cycle, StepType) |>
      group_modify(~ {
        Dat <- .x
        Cap <- if (.y$StepType == "Charge") Dat$SpecificChargeCapacity else Dat$SpecificDischargeCapacity
        Result <- ComputeDqDv(Dat$Voltage, Cap)
        if (.y$StepType == "Discharge") Result$DqDv <- -abs(Result$DqDv)
        Result
      }) |>
      ungroup() |>
      dplyr::filter(!is.na(DqDv), is.finite(DqDv))

    if (nrow(DqDvData) == 0) {
      cat(sprintf("  ! %s — no valid dQ/dV data, skipping\n", CellName))
      return(invisible(NULL))
    }

    P <- ggplot(DqDvData, aes(
      x     = Voltage,
      y     = DqDv,
      group = interaction(Cycle, StepType),
      color = Cycle
    )) +
      geom_line(linewidth = 0.5, alpha = 0.85) +
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
        title    = GetPlotTitle(CellName),
        subtitle = sprintf("%d cycles", NCycles),
        x        = "Voltage (V)",
        y        = "dQ/dV (mAh/g/V)"
      ) +
      Theme_AS() +
      IndividualPlotTheme()

    SaveDir  <- file.path(OutputDir, CellName)
    SavePath <- file.path(SaveDir, "DqDv.png")
    dir.create(SaveDir, recursive = TRUE, showWarnings = FALSE)
    ggsave(SavePath, plot = P, width = PlotWidth, height = PlotHeight, dpi = DPI)
  })

  cat(sprintf("Plotting dQ/dV curves... Done (%d files)\n", length(Files)))
}