# Battery cycling data analysis pipeline
# Processes data from Arbin cycler systems

# --- Libraries and theme -------------------------------------------
source(here::here("R", "DataExtract", "LoadLibs.R"))
source(here::here("R", "Plotting",    "ThemeAS.R"))

# --- Data extraction functions -------------------------------------
source(here::here("R", "DataExtract", "GetFiles.R"))
source(here::here("R", "DataExtract", "ActiveMass.R"))
source(here::here("R", "DataExtract", "LabelSteps.R"))
source(here::here("R", "DataExtract", "ExtractCyclingData.R"))
source(here::here("R", "DataExtract", "PerCycleData.R"))
source(here::here("R", "DataExtract", "ExtractStatsData.R"))
source(here::here("R", "DataExtract", "ExportCycleCurves.R"))

# --- Plotting functions --------------------------------------------
source(here::here("R", "Plotting", "PlotConfig.R"))
source(here::here("R", "Plotting", "SelectFiles.R"))
source(here::here("R", "Plotting", "DischargePerCycle.R"))
source(here::here("R", "Plotting", "DischargeCurves.R"))
source(here::here("R", "Plotting", "ChargeCurves.R"))
source(here::here("R", "Plotting", "VoltagevsTime.R"))
source(here::here("R", "Plotting", "CurrentvsTime.R"))
source(here::here("R", "Plotting", "DqDv.R"))
source(here::here("R", "Plotting", "CEPerCycle.R"))
source(here::here("R", "Plotting", "DCIR.R"))

# --- Run pipeline --------------------------------------------------
FileList     <- GetFiles()
CyclingData  <- ExtractCycleData(FileList)
PerCycleStats <- PerCycleData(CyclingData)
StatsData <- ExtractStatsData(FileList, LabelMap = PlotLabels )
#CurveExportLog <- ExportCycleCurves( CyclingData, LabelMap = PlotLabels, CompleteCyclesOnly = TRUE)



# --- Select files for per-cycle plots -------------------------------
# LEFT BUTTON IS SELECT, RIGHT BUTTON IS CANCEL
SelectedStats <- SelectFiles(PerCycleStats)

# --- Generate plots ------------------------------------------------
CEPerCyclePlot(SelectedStats)
DischargePerCyclePlot(SelectedStats)
DCIRPlot(SelectedStats)
DischargeCurves(CyclingData)
ChargeCurves(CyclingData)


# ----- sometimes i want these -----------------------------------------
#DqDv(CyclingData,Cycles = seq(1, 50, by = 1),SgWindow = 31)
#VoltageVsTime(CyclingData)
#CurrentVsTime(CyclingData)


# ---- EIS -------------------------------------------------------------
#source(here::here("R", "DataExtract", "ExtractEISData.R"))
#source(here::here("R", "Plotting", "NyquistPlots.R"))
#EISData <- ExtractEISData(FileList)
#NyquistPlots(EISData)
#write_xlsx(EISData, file.path("outputs", "EISData.xlsx"))