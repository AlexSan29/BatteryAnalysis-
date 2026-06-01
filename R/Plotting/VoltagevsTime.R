VoltageVsTime <- function(CyclingData,
                            OutputDir  = "outputs/Individual", # nolint: indentation_linter.
                            PaddingPct = 0.03,
                            PlotWidth  = 8,
                            PlotHeight = 5,
                            DPI        = 300) {

  RequiredCols <- c("File", "Cycle", "Voltage", "TestTime", "StepType")
  MissingCols  <- setdiff(RequiredCols, colnames(CyclingData))
  if (length(MissingCols) > 0) {
    stop("CyclingData is missing required columns: ", paste(MissingCols, collapse = ", "))
  }

  PlotData <- CyclingData |>
    filter(!is.na(Cycle), !is.na(Voltage), !is.na(TestTime)) |>
    group_by(File) |>
    filter(Cycle != max(Cycle)) |>
    ungroup() |>
    mutate(TestTime_h = TestTime / 3600)

  Files <- unique(PlotData$File)

  walk(Files, function(Fname) {

    FileData <- PlotData |> filter(File == Fname)
    NCycles  <- n_distinct(FileData$Cycle)
    CellName <- tools::file_path_sans_ext(Fname)

    TimeRange <- range(FileData$TestTime_h, na.rm = TRUE)
    VoltRange <- range(FileData$Voltage,    na.rm = TRUE)
    TimePad   <- diff(TimeRange) * PaddingPct
    VoltPad   <- diff(VoltRange) * PaddingPct

    XLim <- c(TimeRange[1] - TimePad, TimeRange[2] + TimePad)
    YLim <- c(VoltRange[1] - VoltPad, VoltRange[2] + VoltPad)

    P <- ggplot(FileData, aes(
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
        legend.position  = "right",
        legend.direction = "vertical",
        legend.title     = element_text(size = 12),
        legend.text      = element_text(size = 10)
      )

    SaveDir  <- file.path(OutputDir, CellName)
    SavePath <- file.path(SaveDir, "VoltageVsTime.png")
    dir.create(SaveDir, recursive = TRUE, showWarnings = FALSE)
    ggsave(SavePath, plot = P, width = PlotWidth, height = PlotHeight, dpi = DPI)
  })

  cat(sprintf("Plotting voltage vs time... Done (%d files)\n", length(Files)))
}