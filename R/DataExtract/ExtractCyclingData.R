ArbinCols <- c(
  "Test_Time(s)", "Step_Index", "Cycle_Index",
  "Current(A)", "Voltage(V)", "Charge_Capacity(Ah)", "Discharge_Capacity(Ah)", "Internal_Resistance(Ohm)"
)

ExtractCycleData <- function(FileList) {

  CyclingFiles <- FileList[grepl("cycle", basename(FileList), ignore.case = TRUE)]

  if (length(CyclingFiles) == 0) stop("No cycling files found in FileList")

  cat("Loading", length(CyclingFiles), "file(s) from data/raw...\n")

  AllDataList <- map(seq_along(CyclingFiles), function(i) {

    Fname <- basename(CyclingFiles[i])
    Fpath <- CyclingFiles[i]

    SheetNames      <- readxl::excel_sheets(Fpath)
    CandidateSheets <- SheetNames[grepl("^Channel", SheetNames, ignore.case = TRUE)]

    RawList <- map(CandidateSheets, function(Sheet) {
      Dat <- tryCatch(
        readxl::read_excel(Fpath, sheet = Sheet),
        error = function(e) NULL
      )
      if (is.null(Dat)) return(NULL)
      if (all(ArbinCols %in% colnames(Dat))) return(Dat) else return(NULL) # nolint: return_linter.
    })

    RawList <- compact(RawList)

    if (length(RawList) == 0) {
      stop(sprintf("File '%s' contains no sheets with the expected Arbin columns.", Fname))
    }

    Raw <- bind_rows(RawList)

    cat(sprintf("  [%d/%d] %s — %d rows\n", i, length(CyclingFiles), Fname, nrow(Raw)))

    Raw |>
      select(
        TestTime          = `Test_Time(s)`,
        Step              = Step_Index,
        Cycle             = Cycle_Index,
        Current           = `Current(A)`,
        Voltage           = `Voltage(V)`,
        ChargeCapacity    = `Charge_Capacity(Ah)`,
        DischargeCapacity = `Discharge_Capacity(Ah)`,
        InternalResistance = `Internal_Resistance(Ohm)`
      ) |>
      mutate(
        File                      = tools::file_path_sans_ext(Fname),
        ActiveMass                = GetActiveMass(Fname),
        SpecificChargeCapacity    = (ChargeCapacity    * 1000) / ActiveMass,
        SpecificDischargeCapacity = (DischargeCapacity * 1000) / ActiveMass
      ) |>
      LabelSteps()
  })

  AllData <- bind_rows(AllDataList)

  cat(sprintf("Done — %d total rows\n\n", nrow(AllData)))
  AllData
}