ArbinStatsCols <- c(
  "Cycle_Index", "Charge_Capacity(Ah)", "Discharge_Capacity(Ah)",
  "Charge_Energy(Wh)", "Discharge_Energy(Wh)",
  "Charge_Time(s)", "DisCharge_Time(s)", "Vmax_On_Cycle(V)"
)

ExtractStatsData <- function(FileList, OutputDir = "outputs") {

  CyclingFiles <- FileList[grepl("cycle", basename(FileList), ignore.case = TRUE)]

  if (length(CyclingFiles) == 0) stop("No cycling files found in FileList")

  cat("Loading stats from", length(CyclingFiles), "file(s)...\n")

  AllStatsList <- map(seq_along(CyclingFiles), function(i) {

    Fname    <- basename(CyclingFiles[i])
    CellName <- tools::file_path_sans_ext(Fname)

    LastSheet <- length(excel_sheets(CyclingFiles[i]))
    Raw <- tryCatch(
      read_excel(CyclingFiles[i], sheet = LastSheet),
      error = function(e) {
        cat(sprintf("  ! No stats sheet found in %s — skipping\n", Fname))
        NULL
      }
    )

    if (is.null(Raw)) return(NULL)

    MissingCols <- setdiff(ArbinStatsCols, colnames(Raw))
    if (length(MissingCols) > 0) {
      stop(sprintf("File '%s' is missing stats columns: %s", Fname, paste(MissingCols, collapse = ", ")))
    }

    cat(sprintf("  [%d/%d] %s\n", i, length(CyclingFiles), Fname))

    Raw |>
      select(
        Cycle                = Cycle_Index,
        ChargeCapacity_Ah    = `Charge_Capacity(Ah)`,
        DischargeCapacity_Ah = `Discharge_Capacity(Ah)`,
        ChargeEnergy_Wh      = `Charge_Energy(Wh)`,
        DischargeEnergy_Wh   = `Discharge_Energy(Wh)`,
        ChargeTime_s         = `Charge_Time(s)`,
        DischargeTime_s      = `DisCharge_Time(s)`,
        Vmax_V               = `Vmax_On_Cycle(V)`,
        DCIR_Ohm             = `Internal_Resistance(Ohm)`
      ) |>
      mutate(
        File       = CellName,
        ActiveMass = GetActiveMass(Fname),

        SpecificChargeCapacity    = (ChargeCapacity_Ah    * 1000) / ActiveMass,
        SpecificDischargeCapacity = (DischargeCapacity_Ah * 1000) / ActiveMass,
        CoulombicEfficiency = (DischargeCapacity_Ah / ChargeCapacity_Ah) * 100,

        SpecificChargeEnergy    = (ChargeEnergy_Wh    * 1000) / ActiveMass,
        SpecificDischargeEnergy = (DischargeEnergy_Wh * 1000) / ActiveMass,

        ChargeTime_h     = ChargeTime_s    / 3600,
        DischargeTime_h  = DischargeTime_s / 3600,
        TotalCycleTime_h = (ChargeTime_s + DischargeTime_s) / 3600
      ) |>
      select(
        File, Cycle, ActiveMass,
        SpecificChargeCapacity, SpecificDischargeCapacity,CoulombicEfficiency,
        DCIR_Ohm
      )
  })

  StatsData <- bind_rows(AllStatsList)

  dir.create(OutputDir, recursive = TRUE, showWarnings = FALSE)
  SavePath <- file.path(OutputDir, "cycle_stats.xlsx")
  write_xlsx(StatsData, SavePath)

  cat(sprintf("Saved stats: %s\n\n", SavePath))

  StatsData
}