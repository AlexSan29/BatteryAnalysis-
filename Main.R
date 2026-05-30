# theme + libraries
source("R/Plotting/ThemeAS.R")
source("R/DataExtract/LoadLibs.R")

# files in /Raw
source("R/DataExtract/GetFiles.R")
FileList <- GetFiles()

# Source all functions
source("R/DataExtract/ExtractCyclingData.R")
source("R/DataExtract/ActiveMass.R")
source("R/DataExtract/LabelSteps.R")
source("R/DataExtract/PerCycleData.R")
source("R/DataExtract/ExtractStatsData.R")

# Get cycling data and per cycle data
CyclingData  <- ExtractCycleData(FileList)
PerCycleData <- PerCycleData(CyclingData)
StatsData    <- ExtractStatsData(FileList)


source("R/Plotting/DischargePerCycle.R")
source("R/Plotting/ResistancePerCycle.R")
source("R/Plotting/DischargeCurves.R")
source("R/Plotting/ChargeCurves.R")
source("R/Plotting/VoltageVsTime.R")
DischargeCyclePlot  <- DischargePerCyclePlot(PerCycleData)
# ResistanceCyclePlot <- ResistancePerCyclePlot(PerCycleData)
PlotVoltageCapacity(CyclingData)
PlotChargeCurves(CyclingData)
PlotVoltageTime(CyclingData)
