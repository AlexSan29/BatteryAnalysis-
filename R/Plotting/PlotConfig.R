PlotColors <- c(
  "DOE_Cycle_2_1"  = "#00BFC4",
  "DOE_Cycle_2_2"  = "#C77CFF",
  "DOE_Cycle_2_3"  = "#FF61CC",
  "DOE_Cycle_2_4"  = "#476a51",
  "DOE_Cycle_2_5"  = "#619CFF",
  
  "LMRO_Cycle_1"   = "#f7572a",
  "LMRO_Cycle_2"   = "#48d80e",
  "LMRO_Cycle_3"   = "#f53520",

  "LMRO_Cycle_4"   = "#74dea2",
  "LMRO_Cycle_5"   = "#22c92f",
  "LMRO_Cycle_6"   = "#22c92f",
  "LMRO_Cycle_7"   = "#ff0000",

  "LMRO_Cycle_8"   = "#7a0eff",

  "LMRO_Cycle_9"   = "#ff0000",
  "LMRO_Cycle_16"  = "#22c92f",
  "LMRO_Cycle_17"  = "#f4166b",
  "LMRO_Cycle_18"  = "#1a7004",


  "811_Cycle_11"   = "#000000",
  "811_Cycle_12"   = "#0059ff",
  "811_Cycle_13"   = "#0059ff",
  "811_Cycle_14"   = "#000000",
  "811_Cycle_15"   = "#0059ff",

  "MC_Cycle_LMRO_3"      = "#22c92f",
  "MC_Cycle_LMRO_4"      = "#22c92f"

  


)

PlotLabels <- c(
  "DOE_Cycle_2_1"  = "111 -> SEM/EDS ",
  "DOE_Cycle_2_2"  = "111 Idle at 100C",
  "DOE_Cycle_2_3"  = "811 -> SEM/EDS ",
  "DOE_Cycle_2_4"  = "111 Rate Capability",
  "DOE_Cycle_2_5"  = "111 + VC -> SEM/EDS",


  "LMRO_Cycle_1"   = "Cell 1\n N:P 0.94",
  "LMRO_Cycle_2"   = "Cell 2\n N:P 0.87",
  "LMRO_Cycle_3"   = "Cell 3 Gr || Ru",
  "LMRO_Cycle_4"   = "4 LMRO Conditioning (no 2V)",
  "LMRO_Cycle_5"   = "5 LMRO Conditioning",
  "LMRO_Cycle_6"   = "6 LMRO Conditioning",
  "LMRO_Cycle_7"   = "7 LMRO Full Window ",
  "LMRO_Cycle_8"   = "Cell 3 \n N:P 1.5",
  "LMRO_Cycle_9"   = "09 LMRO Full Window",
  "LMRO_Cycle_16"  = "16 LMRO Conditioning",
  "LMRO_Cycle_17"  = "17 LMRO Full Window New Material",
  "LMRO_Cycle_18"  = "18 LMRO Conditioning New Material",

  "811_Cycle_11"   = "11 NMC811 Full Window",
  "811_Cycle_12"   = "12 NMC811 Conditioning",
  "811_Cycle_13"   = "13 NMC811 Conditioning",
  "811_Cycle_14"   = "14 NMC811 Full Window",
  "811_Cycle_15"   = "15 NMC811 Conditioning",

  "MC_Cycle_LMRO_3"      = "LMRO ",
  "MC_Cycle_LMRO_4"      = "LMRO "

  
)

PlotTitles <- c(
  "DOE_Cycle_2_1"  = "111 -> SEM/EDS",
  "DOE_Cycle_2_2"  = "111 Idle at 100C",
  "DOE_Cycle_2_3"  = "811 -> SEM/EDS",
  "DOE_Cycle_2_4"  = "111 Rate Capability",
  "DOE_Cycle_2_5"  = "111 + VC -> SEM/EDS",
  
  "LMRO_Cycle_1"   = "Cell 1, N:P 0.94",
  "LMRO_Cycle_2"   = "Cell 2, N:P 0.87",
  "LMRO_Cycle_3"   = "Cell 3 Gr || Ru",
  "LMRO_Cycle_4"   = "4 LMRO Conditioning(no 2V)",
  "LMRO_Cycle_5"   = "5 LMRO Conditioning",
  "LMRO_Cycle_6"   = "6 LMRO Conditioning",
  "LMRO_Cycle_7"   = "7 LMRO Full Window",
  "LMRO_Cycle_8"   = "Cell 3, N:P 1.5",
  "LMRO_Cycle_9"   = "09 LMRO Full Window",
  "LMRO_Cycle_16"  = "16 LMRO Conditioning",
  "LMRO_Cycle_17"  = "17 LMRO Full Window New Material",
  "LMRO_Cycle_18"  = "18 LMRO Conditioning New Material",
  
  "811_Cycle_11"   = "11 NMC811 Full Window",
  "811_Cycle_12"   = "12 NMC811 Conditioning",
  "811_Cycle_13"   = "13 NMC811 Conditioning",
  "811_Cycle_14"   = "14 NMC811 Full Window",
  "811_Cycle_15"   = "15 NMC811 Conditioning",

  
  "MC_Cycle_LMRO_3"      = "LMRO",
  "MC_Cycle1_LMRO_4"      = "LMRO"
)

GetPlotTitle <- function(CellName) {
  if (CellName %in% names(PlotTitles)) PlotTitles[[CellName]] else CellName
}

VoltageYLim   <- c(1.3, 4.6)
VoltageYBreak <- 0.3

CycleColorScale <- function(NCycles) {
  scale_color_viridis_c(
    name   = "Cycle",
    option = "viridis",
    breaks = scales::pretty_breaks(n = max(min(NCycles, 6), 2))
  )
}

StageColors <- c(
  "1" = "#3B82C4",
  "2" = "#4DAF7C",
  "3" = "#E8A33D",
  "4" = "#D64545"
)

StageLabels <- c(
  "1" = "S1 (2.0 - 4.2V)",
  "2" = "S2 (2.0 - 4.3V)",
  "3" = "S3 (2.0 - 4.4V)",
  "4" = "S4 (2.0 - 4.6V)"
)

StageColorScale <- function() {
  scale_color_manual(
    name   = "Stage",
    values = StageColors,
    labels = StageLabels
  )
}


IndividualPlotTheme <- function() {
  theme(
    legend.position  = "right",
    legend.direction = "vertical",
    legend.title     = element_text(size = 12),
    legend.text      = element_text(size = 10)
  )
}