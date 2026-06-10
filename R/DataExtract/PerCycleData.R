PerCycleData <- function(CyclingData) {

  LastCycles <- CyclingData |>
    dplyr::filter(!is.na(Cycle)) |>
    group_by(File) |>
    summarise(DroppedCycle = max(Cycle), .groups = "drop")

  cat("Dropping incomplete last cycle per file:\n")
  walk2(LastCycles$File, LastCycles$DroppedCycle,
        ~ cat(sprintf("  %s — cycle %d removed\n", .x, .y)))
  cat("\n")

  CyclingData |>
    dplyr::filter(!is.na(Cycle)) |>
    group_by(File) |>
    dplyr::filter(Cycle != max(Cycle)) |>
    ungroup() |>
    group_by(File, Cycle) |>
    summarise(
      MaxSpecificChargeCapacity    = suppressWarnings(max(SpecificChargeCapacity[StepType == "Charge"],    na.rm = TRUE)),
      MaxSpecificDischargeCapacity = suppressWarnings(max(SpecificDischargeCapacity[StepType == "Discharge"], na.rm = TRUE)),
      DischargeFirstVoltage = {
        V <- Voltage[StepType == "Discharge"]
        if (length(V) > 0) V[1] else NA_real_
      },
      DischargeCurrent = {
        I <- Current[StepType == "Discharge"]
        if (length(I) > 0) I[1] else NA_real_
      },
      RestLastVoltage = {
        IdxDis <- which(StepType == "Discharge")[1]
        if (is.na(IdxDis)) {
          NA_real_
        } else {
          VRestBefore <- Voltage[seq_len(IdxDis - 1)][StepType[seq_len(IdxDis - 1)] == "Rest"]
          if (length(VRestBefore) > 0) VRestBefore[length(VRestBefore)] else NA_real_
        }
      },
      .groups = "drop"
    ) |>
    mutate(
      CoulombicEfficiency = (MaxSpecificDischargeCapacity / MaxSpecificChargeCapacity) * 100,
      Resistance          = (RestLastVoltage - DischargeFirstVoltage) / abs(DischargeCurrent)
    ) |>
    select(
      File, Cycle,
      MaxSpecificChargeCapacity, MaxSpecificDischargeCapacity,
      CoulombicEfficiency, Resistance
    )
}