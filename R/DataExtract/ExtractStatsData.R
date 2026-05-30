# =============================================================================
# ExtractStatsData.R
# Goal: Read the per-cycle statistics sheet (last sheet) from each Arbin file
#       Normalizes capacity and energy by active mass
#       Converts time from seconds to hours
# Requires: ActiveMass.R, GetFiles.R
# =============================================================================

ARBIN_STATS_COLS <- c(
  "Cycle_Index", "Charge_Capacity(Ah)", "Discharge_Capacity(Ah)",
  "Charge_Energy(Wh)", "Discharge_Energy(Wh)",
  "Charge_Time(s)", "DisCharge_Time(s)", "Vmax_On_Cycle(V)"
)

ExtractStatsData <- function(FileList, OutputDir = "outputs") {
  
  CyclingFiles <- FileList[grepl("cycle", basename(FileList), ignore.case = TRUE)]
  
  cat("=== ExtractStatsData ===\n")
  cat("Found", length(CyclingFiles), "cycling file(s) to load\n\n")
  
  if (length(CyclingFiles) == 0) stop("No cycling files found in FileList")
  
  AllStatsList <- map(seq_along(CyclingFiles), function(i) {
    
    fname    <- basename(CyclingFiles[i])
    CellName <- tools::file_path_sans_ext(fname)
    cat(sprintf("  [%d/%d] Reading stats: %s\n", i, length(CyclingFiles), fname))
    
    # Always grab the last sheet (stats sheet moves as data sheets are added)
    LastSheet <- length(excel_sheets(CyclingFiles[i]))
    raw <- tryCatch(
      read_excel(CyclingFiles[i], sheet = LastSheet),
      error = function(e) {
        cat(sprintf("    ! No stats sheet found in %s — skipping\n", fname))
        return(NULL)
      }
    )
    
    if (is.null(raw)) return(NULL)
    
    # Validate expected columns are present
    MissingCols <- setdiff(ARBIN_STATS_COLS, colnames(raw))
    if (length(MissingCols) > 0) {
      stop(sprintf("File '%s' is missing stats columns: %s", fname, paste(MissingCols, collapse = ", ")))
    }
    
    raw |>
      select(
        Cycle                = Cycle_Index,
        ChargeCapacity_Ah    = `Charge_Capacity(Ah)`,
        DischargeCapacity_Ah = `Discharge_Capacity(Ah)`,
        ChargeEnergy_Wh      = `Charge_Energy(Wh)`,
        DischargeEnergy_Wh   = `Discharge_Energy(Wh)`,
        ChargeTime_s         = `Charge_Time(s)`,
        DischargeTime_s      = `DisCharge_Time(s)`,
        Vmax_V               = `Vmax_On_Cycle(V)`
      ) |>
      mutate(
        File       = CellName,
        ActiveMass = GetActiveMass(fname),
        
        # Normalize capacity: Ah -> mAh/g
        SpecificChargeCapacity    = (ChargeCapacity_Ah    * 1000) / ActiveMass,
        SpecificDischargeCapacity = (DischargeCapacity_Ah * 1000) / ActiveMass,
        
        # Normalize energy: Wh -> mWh/g
        SpecificChargeEnergy    = (ChargeEnergy_Wh    * 1000) / ActiveMass,
        SpecificDischargeEnergy = (DischargeEnergy_Wh * 1000) / ActiveMass,
        
        # Convert time: seconds -> hours
        ChargeTime_h     = ChargeTime_s    / 3600,
        DischargeTime_h  = DischargeTime_s / 3600,
        TotalCycleTime_h = (ChargeTime_s + DischargeTime_s) / 3600
      ) |>
      select(
        File, Cycle, ActiveMass,
        SpecificChargeCapacity, SpecificDischargeCapacity,
        SpecificChargeEnergy, SpecificDischargeEnergy,
        ChargeTime_h, DischargeTime_h, TotalCycleTime_h,
        Vmax_V
      )
  })
  
  StatsData <- bind_rows(AllStatsList)
  
  cat(sprintf("\nDone — %d cycle rows across %d file(s)\n\n",
              nrow(StatsData), n_distinct(StatsData$File)))
  
  dir.create(OutputDir, recursive = TRUE, showWarnings = FALSE)
  SavePath <- file.path(OutputDir, "cycle_stats.xlsx")
  write_xlsx(StatsData, SavePath)
  cat(sprintf("Saved: %s\n\n", SavePath))
  
  return(StatsData)
}