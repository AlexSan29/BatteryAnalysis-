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

# --- Plotting functions --------------------------------------------
source(here::here("R", "Plotting", "DischargePerCycle.R"))
source(here::here("R", "Plotting", "ResistancePerCycle.R"))
source(here::here("R", "Plotting", "DischargeCurves.R"))
source(here::here("R", "Plotting", "ChargeCurves.R"))
source(here::here("R", "Plotting", "VoltagevsTime.R"))
source(here::here("R", "Plotting", "DqDv.R"))

# --- Run pipeline --------------------------------------------------
FileList     <- GetFiles()
CyclingData  <- ExtractCycleData(FileList)
PerCycleStats <- PerCycleData(CyclingData)
StatsData    <- ExtractStatsData(FileList)

# --- Generate plots ------------------------------------------------
DischargePerCyclePlot(PerCycleStats)
DischargeCurves(CyclingData)
ChargeCurves(CyclingData)
VoltageVsTime(CyclingData)
DqDv(CyclingData)
