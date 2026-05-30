PerCycleData <- function(CyclingData) {
  
  cat("=== PerCycleData ===\n")
  cat("Files    :", n_distinct(CyclingData$File), "\n")
  cat("Cycles   :", n_distinct(CyclingData$Cycle), "\n")
  cat("Total rows:", nrow(CyclingData), "\n\n")
  
  # Remove last cycle per file (likely still in progress)
  LastCycles <- CyclingData |>
    filter(!is.na(Cycle)) |>
    group_by(File) |>
    summarise(DroppedCycle = max(Cycle), .groups = "drop")
  
  cat("Dropping last cycle per file (may be incomplete):\n")
  walk2(LastCycles$File, LastCycles$DroppedCycle, ~ cat(sprintf("  %s — cycle %d removed\n", .x, .y)))
  cat("\n")
  
  result <- CyclingData |>
    filter(!is.na(Cycle)) |>
    group_by(File) |>
    filter(Cycle != max(Cycle)) |>
    ungroup() |>
    group_by(File, Cycle) |>
    summarise(
      
      # Capacities — max value within each step type
      MaxSpecificChargeCapacity    = suppressWarnings(max(SpecificChargeCapacity[StepType    == "Charge"],    na.rm = TRUE)),
      MaxSpecificDischargeCapacity = suppressWarnings(max(SpecificDischargeCapacity[StepType == "Discharge"], na.rm = TRUE)),
      
      # Resistance components: rest right before discharge
      DischargeFirstVoltage = {
        v <- Voltage[StepType == "Discharge"]
        if (length(v) > 0) v[1] else NA_real_
      },
      DischargeCurrent = {
        i <- Current[StepType == "Discharge"]
        if (length(i) > 0) i[1] else NA_real_
      },
      RestLastVoltage = {
        # index (within this File+Cycle group) of the first discharge row
        idx_dis <- which(StepType == "Discharge")[1]
        
        if (is.na(idx_dis)) {
          NA_real_
        } else {
          # only look at rows before discharge starts
          v_rest_before <- Voltage[seq_len(idx_dis - 1)][StepType[seq_len(idx_dis - 1)] == "Rest"]
          if (length(v_rest_before) > 0) v_rest_before[length(v_rest_before)] else NA_real_
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
  
  cat("Output:", nrow(result), "cycle(s) across", n_distinct(result$File), "file(s)\n\n")
  
  return(result)
}