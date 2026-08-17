ArbinStatsCols <- c(
  "Cycle_Index", "Charge_Capacity(Ah)", "Discharge_Capacity(Ah)",
  "Charge_Energy(Wh)", "Discharge_Energy(Wh)",
  "Internal_Resistance(Ohm)",
  "Charge_Time(s)", "DisCharge_Time(s)", "Vmax_On_Cycle(V)"
)

MakeExcelSheetNames <- function(FileNames, Prefix = "Cell_") {
  CleanNames <- FileNames |>
    stringr::str_replace_all("[\\\\/:?*\\[\\]]", "_") |>
    stringr::str_squish()

  CleanNames[CleanNames == ""] <- "Unnamed"
  CleanNames <- paste0(Prefix, CleanNames)
  SequenceNumber <- ave(seq_along(CleanNames), CleanNames, FUN = seq_along)
  Suffix <- ifelse(SequenceNumber == 1, "", paste0("_", SequenceNumber))

  paste0(
    stringr::str_sub(CleanNames, 1, 31 - nchar(Suffix)),
    Suffix
  )
}

ResolveExcelLabels <- function(FileNames, LabelMap = NULL) {
  if (is.null(LabelMap) && exists("PlotLabels", inherits = TRUE)) {
    LabelMap <- get("PlotLabels", inherits = TRUE)
  }

  Labels <- FileNames
  if (!is.null(LabelMap)) {
    Matches <- unname(LabelMap[FileNames])
    HasMatch <- !is.na(Matches) & nzchar(Matches)
    Labels[HasMatch] <- Matches[HasMatch]
  }

  Labels <- Labels |>
    stringr::str_replace_all("[\\r\\n]+", " ") |>
    stringr::str_squish()

  # Excel columns must still be unique if two files share the same plot label.
  make.unique(Labels, sep = " (") |>
    stringr::str_replace(" \\(([0-9]+)$", " (\\1)")
}

MakeWideSheet <- function(Data, ValueColumn) {
  Data |>
    dplyr::select(Cycle, Label, dplyr::all_of(ValueColumn)) |>
    dplyr::distinct(Cycle, Label, .keep_all = TRUE) |>
    tidyr::pivot_wider(
      names_from = Label,
      values_from = dplyr::all_of(ValueColumn)
    ) |>
    dplyr::arrange(Cycle)
}

ExtractStatsData <- function(FileList,
                             OutputDir = "outputs",
                             IncludeFileSheets = TRUE,
                             LabelMap = NULL) {

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

        # ActiveMass is in grams, giving specific energy in Wh/kg.
        SpecificChargeEnergy    = (ChargeEnergy_Wh    * 1000) / ActiveMass,
        SpecificDischargeEnergy = (DischargeEnergy_Wh * 1000) / ActiveMass,
        EnergyEfficiency = (DischargeEnergy_Wh / ChargeEnergy_Wh) * 100,

        ChargeTime_h     = ChargeTime_s    / 3600,
        DischargeTime_h  = DischargeTime_s / 3600,
        TotalCycleTime_h = (ChargeTime_s + DischargeTime_s) / 3600
      ) |>
      mutate(
        across(
          c(CoulombicEfficiency, EnergyEfficiency),
          ~ ifelse(is.finite(.x), .x, NA_real_)
        )
      ) |>
      select(
        File, Cycle, ActiveMass,
        ChargeCapacity_Ah, DischargeCapacity_Ah,
        SpecificChargeCapacity, SpecificDischargeCapacity,
        CoulombicEfficiency,
        ChargeEnergy_Wh, DischargeEnergy_Wh,
        SpecificChargeEnergy, SpecificDischargeEnergy,
        EnergyEfficiency,
        DCIR_Ohm,
        ChargeTime_h, DischargeTime_h, TotalCycleTime_h,
        Vmax_V
      )
  })

  StatsData <- bind_rows(AllStatsList) |>
    arrange(File, Cycle)

  if (nrow(StatsData) == 0) {
    stop("No usable Arbin cycle-stat rows were found.")
  }

  FileNames <- unique(StatsData$File)
  LabelLookup <- stats::setNames(
    ResolveExcelLabels(FileNames, LabelMap),
    FileNames
  )

  StatsData <- StatsData |>
    mutate(Label = unname(LabelLookup[File])) |>
    relocate(Label, .after = File)

  WorkbookSheets <- list(
    CycleStats_Long = StatsData,
    DischargeCapacity = MakeWideSheet(StatsData, "SpecificDischargeCapacity"),
    ChargeCapacity = MakeWideSheet(StatsData, "SpecificChargeCapacity"),
    DCIR = MakeWideSheet(StatsData, "DCIR_Ohm"),
    DischargeEnergy = MakeWideSheet(StatsData, "DischargeEnergy_Wh"),
    ChargeEnergy = MakeWideSheet(StatsData, "ChargeEnergy_Wh"),
    SpecificDischargeEnergy = MakeWideSheet(StatsData, "SpecificDischargeEnergy"),
    SpecificChargeEnergy = MakeWideSheet(StatsData, "SpecificChargeEnergy"),
    CoulombicEfficiency = MakeWideSheet(StatsData, "CoulombicEfficiency"),
    EnergyEfficiency = MakeWideSheet(StatsData, "EnergyEfficiency")
  )

  if (IncludeFileSheets) {
    SplitStats <- split(StatsData, StatsData$File)
    names(SplitStats) <- MakeExcelSheetNames(names(SplitStats))
    WorkbookSheets <- c(WorkbookSheets, SplitStats)
  }

  dir.create(OutputDir, recursive = TRUE, showWarnings = FALSE)
  SavePath <- file.path(OutputDir, "cycle_stats.xlsx")
  writexl::write_xlsx(WorkbookSheets, SavePath)

  cat(sprintf(
    "Saved stats workbook: %s (%d sheets)\n\n",
    SavePath, length(WorkbookSheets)
  ))

  StatsData
}