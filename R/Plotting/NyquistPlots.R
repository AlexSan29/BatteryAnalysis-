NyquistPlots <- function(EISData,
                             OutputDir  = "outputs/Individual", # nolint: indentation_linter.
                             PlotWidth  = 8,
                             PlotHeight = 5.5,
                             DPI        = 300) {

  RequiredCols <- c("CellNum", "Stage", "ChargeState", "ReZ", "NegImZ")
  MissingCols  <- setdiff(RequiredCols, colnames(EISData))
  if (length(MissingCols) > 0) {
    stop("EISData is missing required columns: ", paste(MissingCols, collapse = ", "))
  }

  Combos <- EISData |> distinct(CellNum, ChargeState, Material)

  XMax <- 15
  YMax <- 15

  pwalk(Combos, function(CellNum, ChargeState, Material) {

    ThisCell     <- CellNum
    ThisState    <- ChargeState
    ThisMaterial <- Material

    StateData      <- EISData |> filter(CellNum == ThisCell, ChargeState == ThisState, Material == ThisMaterial)
    CellFolderName <- sprintf("%s_Cycle_%s", ThisMaterial, ThisCell)
    StateLabel     <- if (ThisState == "TOC") "Top of Charge" else "Bottom of Charge"

    P <- ggplot(StateData, aes(
      x     = ReZ,
      y     = NegImZ,
      group = factor(Stage),
      color = factor(Stage)
    )) +
      geom_point(size = 1.4, alpha = 0.85) +
      StageColorScale() +
      coord_fixed(ratio = 1) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6), 
      #limits = c(0, XMax)
      ) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6), 
      #limits = c(0, YMax)
      ) +
      labs(
        title    = GetPlotTitle(CellFolderName),
        subtitle = StateLabel,
        x        = "Re(Z) (\u03a9)",
        y        = "-Im(Z) (\u03a9)"
      ) +
      Theme_AS() +
      IndividualPlotTheme() + 
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal"
      ) 

    SaveDir  <- file.path(OutputDir, CellFolderName)
    SavePath <- file.path(SaveDir, sprintf("Nyquist_%s.png", ThisState))
    dir.create(SaveDir, recursive = TRUE, showWarnings = FALSE)
    ggsave(SavePath, plot = P, width = PlotWidth, height = PlotHeight, dpi = DPI)
  })

  cat(sprintf("Plotting Nyquist plots... Done (%d plot(s))\n", nrow(Combos)))
}