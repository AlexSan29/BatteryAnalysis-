ResolveCurveLabel <- function(FileName, LabelMap = NULL) {
  if (is.null(LabelMap) && exists("PlotLabels", inherits = TRUE)) {
    LabelMap <- get("PlotLabels", inherits = TRUE)
  }

  Label <- FileName
  if (!is.null(LabelMap) && FileName %in% names(LabelMap)) {
    Label <- unname(LabelMap[[FileName]])
  }

  Label |>
    stringr::str_replace_all("[\r\n]+", " ") |>
    stringr::str_squish()
}

MakeSafePathName <- function(Name) {
  Name |>
    stringr::str_replace_all("[<>:\"/\\\\|?*]", "_") |>
    stringr::str_replace_all("[\r\n]+", " ") |>
    stringr::str_squish()
}

PadCurveColumns <- function(ColumnList) {
  if (length(ColumnList) == 0) return(tibble::tibble())

  MaxLength <- max(lengths(ColumnList))
  Padded <- lapply(ColumnList, function(Column) {
    length(Column) <- MaxLength
    Column
  })

  tibble::as_tibble(Padded, .name_repair = "minimal")
}

MakeCombinedWide <- function(CurveData) {
  ColumnList <- list()

  for (CycleNumber in sort(unique(CurveData$Cycle))) {
    ThisCycle <- CurveData |>
      dplyr::filter(Cycle == CycleNumber) |>
      dplyr::arrange(TestTime)

    Charge <- ThisCycle |>
      dplyr::filter(StepType == "Charge")
    Discharge <- ThisCycle |>
      dplyr::filter(StepType == "Discharge")

    Prefix <- paste0("Cycle ", CycleNumber)
    ColumnList[[paste0(Prefix, " Charge Capacity (mAh/g)")]] <-
      Charge$SpecificChargeCapacity
    ColumnList[[paste0(Prefix, " Charge Voltage (V)")]] <- Charge$Voltage
    ColumnList[[paste0(Prefix, " Discharge Capacity (mAh/g)")]] <-
      Discharge$SpecificDischargeCapacity
    ColumnList[[paste0(Prefix, " Discharge Voltage (V)")]] <- Discharge$Voltage
  }

  PadCurveColumns(ColumnList)
}

ExportCycleCurves <- function(CyclingData,
                              OutputDir = "outputs/Individual",
                              Cycles = NULL,
                              LabelMap = NULL,
                              CompleteCyclesOnly = FALSE) {
  RequiredCols <- c(
    "File", "Cycle", "TestTime", "StepType", "Voltage", "Current",
    "SpecificChargeCapacity", "SpecificDischargeCapacity"
  )
  MissingCols <- setdiff(RequiredCols, colnames(CyclingData))
  if (length(MissingCols) > 0) {
    stop(
      "CyclingData is missing required columns: ",
      paste(MissingCols, collapse = ", ")
    )
  }

  CurveData <- CyclingData |>
    dplyr::filter(
      !is.na(File), !is.na(Cycle), !is.na(Voltage),
      StepType %in% c("Charge", "Discharge")
    ) |>
    dplyr::mutate(
      PlotCapacity_mAh_g = dplyr::if_else(
        StepType == "Charge",
        SpecificChargeCapacity,
        SpecificDischargeCapacity
      )
    ) |>
    dplyr::filter(!is.na(PlotCapacity_mAh_g))

  if (!is.null(Cycles)) {
    CurveData <- CurveData |>
      dplyr::filter(Cycle %in% Cycles)
  }

  if (CompleteCyclesOnly) {
    CurveData <- CurveData |>
      dplyr::group_by(File, Cycle) |>
      dplyr::filter(
        any(StepType == "Charge"),
        any(StepType == "Discharge")
      ) |>
      dplyr::ungroup()
  }

  if (nrow(CurveData) == 0) {
    stop("No charge or discharge curve points remain after filtering.")
  }

  Files <- unique(CurveData$File)
  ExportLog <- purrr::map_dfr(Files, function(FileName) {
    FileData <- CurveData |>
      dplyr::filter(File == FileName) |>
      dplyr::arrange(Cycle, TestTime)

    DisplayLabel <- ResolveCurveLabel(FileName, LabelMap)
    SafeFileName <- MakeSafePathName(FileName)
    CellDir <- file.path(OutputDir, SafeFileName)
    SavePath <- file.path(CellDir, paste0("CycleData_", SafeFileName, ".xlsx"))

    Workbook <- list(
      Curves_Wide = MakeCombinedWide(FileData)
    )

    dir.create(CellDir, recursive = TRUE, showWarnings = FALSE)
    writexl::write_xlsx(Workbook, SavePath)

    tibble::tibble(
      File = FileName,
      Label = DisplayLabel,
      CyclesExported = dplyr::n_distinct(FileData$Cycle),
      Workbook = SavePath
    )
  })

  cat(sprintf(
    "Exported %d cycle-curve workbook(s) to %s\n",
    nrow(ExportLog), OutputDir
  ))

  ExportLog
}