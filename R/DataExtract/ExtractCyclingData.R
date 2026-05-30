# Expected Arbin column names
ARBIN_COLS <- c(
  "Test_Time(s)", "Step_Index", "Cycle_Index",
  "Current(A)", "Voltage(V)", "Charge_Capacity(Ah)", "Discharge_Capacity(Ah)"
)

ExtractCycleData <- function(FileList) {
  
  CyclingFiles <- FileList[grepl("cycle", basename(FileList), ignore.case = TRUE)]
  
  cat("=== ExtractCycleData ===\n")
  cat("Found", length(CyclingFiles), "cycling file(s) to load\n\n")
  
  if (length(CyclingFiles) == 0) stop("No cycling files found in FileList")
  
  AllDataList <- purrr::map(seq_along(CyclingFiles), function(i) {
    
    fname <- basename(CyclingFiles[i])
    fpath <- CyclingFiles[i]
    
    cat(sprintf("  [%d/%d] Loading: %s\n", i, length(CyclingFiles), fname))
    
    # Get all sheet names
    sheet_names <- readxl::excel_sheets(fpath)
    
    # Usually sheet 1 is summary/info, and data starts on sheet 2
    candidate_sheets <- sheet_names[grepl("^Channel", sheet_names, ignore.case = TRUE)]
    
    # Read each candidate sheet; keep only sheets that contain Arbin data columns
    raw_list <- purrr::map(candidate_sheets, function(sh) {
      dat <- tryCatch(
        readxl::read_excel(fpath, sheet = sh),
        error = function(e) NULL
      )
      
      if (is.null(dat)) return(NULL)
      
      if (all(ARBIN_COLS %in% colnames(dat))) {
        cat(sprintf("      Included sheet: %s (%d rows)\n", sh, nrow(dat)))
        return(dat)
      } else {
        cat(sprintf("      Skipped sheet: %s (not a data sheet)\n", sh))
        return(NULL)
      }
    })
    
    raw_list <- purrr::compact(raw_list)
    
    if (length(raw_list) == 0) {
      stop(sprintf("File '%s' contains no sheets with the expected Arbin columns.", fname))
    }
    
    raw <- dplyr::bind_rows(raw_list)
    
    cat(sprintf("  [%d/%d] Done: %s — %d total rows\n", i, length(CyclingFiles), fname, nrow(raw)))
    
    raw |>
      dplyr::select(
        TestTime          = `Test_Time(s)`,
        Step              = Step_Index,
        Cycle             = Cycle_Index,
        Current           = `Current(A)`,
        Voltage           = `Voltage(V)`,
        ChargeCapacity    = `Charge_Capacity(Ah)`,
        DischargeCapacity = `Discharge_Capacity(Ah)`
      ) |>
      dplyr::mutate(
        File                      = tools::file_path_sans_ext(fname),
        ActiveMass                = GetActiveMass(fname),
        SpecificChargeCapacity    = (ChargeCapacity    * 1000) / ActiveMass,
        SpecificDischargeCapacity = (DischargeCapacity * 1000) / ActiveMass
      ) |>
      LabelSteps()
  })
  
  AllData <- dplyr::bind_rows(AllDataList)
  
  cat(sprintf("\nDone — %d total rows across %d file(s)\n\n", nrow(AllData), length(CyclingFiles)))
  
  return(AllData)
}