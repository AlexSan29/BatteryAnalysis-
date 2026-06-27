PlotColors <- c(
  "LMRO_Cycle_1"   = "#f7572a",
  "LMRO_Cycle_2"   = "#48d80e",
  "LMRO_Cycle_3"   = "#f53520",
  "LMRO_Cycle_4"   = "#c9b3e4",
  "LMRO_Cycle_5"   = "#7a0eff",
  "LMRO_Cycle_6"   = "#7a0eff",
  "LMRO_Cycle_7"   = "#f7572a",
  "LMRO_Cycle_8"   = "#7a0eff",
  "LMRO_Cycle_9"   = "#f7572a",
  "LMRO_Cycle_11"  = "#f7572a",
  "LMRO_Cycle_12"  = "#7a0eff",
  "LMRO_Cycle_13"  = "#f7572a",
  "DOE_Cycle_2_1"  = "#00BFC4",
  "DOE_Cycle_2_2"  = "#C77CFF",
  "DOE_Cycle_2_3"  = "#FF61CC",
  "DOE_Cycle_2_4"  = "#476a51",
  "DOE_Cycle_2_5"  = "#619CFF"
)

PlotLabels <- c(
  "LMRO_Cycle_1"   = "Cell 1\n N:P 0.94",
  "LMRO_Cycle_2"   = "Cell 2\n N:P 0.87",
  "LMRO_Cycle_3"   = "Cell 3 Gr || Ru",
  "LMRO_Cycle_4"   = "Cell 1\nConditioning",
  "LMRO_Cycle_5"   = "Cell 2\nConditioning",
  "LMRO_Cycle_6"   = "Cell 3\nConditioning",
  "LMRO_Cycle_7"   = "Cell 4\nFull Window ",
  "LMRO_Cycle_8"   = "Cell 3 \n N:P 1.5",
  "LMRO_Cycle_9"   = "Cell 9\nLMRO Full Window",
  "LMRO_Cycle_11"  = "Cell 11\nNMC811 Full Window",
  "LMRO_Cycle_12"  = "Cell 12\nNMC811 Conditioning",
  "LMRO_Cycle_13"  = "Cell 13\nNMC811 Conditioning",
  "DOE_Cycle_2_1"  = "111 -> SEM/EDS ",
  "DOE_Cycle_2_2"  = "111 Idle at 100C",
  "DOE_Cycle_2_3"  = "811 -> SEM/EDS ",
  "DOE_Cycle_2_4"  = "111 Rate Capability",
  "DOE_Cycle_2_5"  = "111 + VC -> SEM/EDS"
)

PlotTitles <- c(
  "LMRO_Cycle_1"   = "Cell 1, N:P 0.94",
  "LMRO_Cycle_2"   = "Cell 2, N:P 0.87",
  "LMRO_Cycle_3"   = "Cell 3 Gr || Ru",
  "LMRO_Cycle_4"   = "Cell 1 Conditioning",
  "LMRO_Cycle_5"   = "Cell 2 Conditioning",
  "LMRO_Cycle_6"   = "Cell 3 Conditioning",
  "LMRO_Cycle_7"   = "Cell 4 Full Window",
  "LMRO_Cycle_8"   = "Cell 3, N:P 1.5",
  "LMRO_Cycle_9"   = "Cell 9 LMRO Full Window",
  "LMRO_Cycle_11"  = "Cell 11 NMC811 Full Window",
  "LMRO_Cycle_12"  = "Cell 12 NMC811 Conditioning",
  "LMRO_Cycle_13"  = "Cell 13 NMC811 Conditioning",
  "DOE_Cycle_2_1"  = "111 -> SEM/EDS",
  "DOE_Cycle_2_2"  = "111 Idle at 100C",
  "DOE_Cycle_2_3"  = "811 -> SEM/EDS",
  "DOE_Cycle_2_4"  = "111 Rate Capability",
  "DOE_Cycle_2_5"  = "111 + VC -> SEM/EDS"
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

IndividualPlotTheme <- function() {
  theme(
    legend.position  = "right",
    legend.direction = "vertical",
    legend.title     = element_text(size = 12),
    legend.text      = element_text(size = 10)
  )
}