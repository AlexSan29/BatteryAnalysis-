# =============================================================================
# LabelSteps.R
# Labels current direction and identifies capacity-producing blocks that should
# appear in charge/discharge curves.
# =============================================================================

LabelSteps <- function(df, CurrentTolerance = 1e-6) {
  df |>
    mutate(
      StepType = case_when(
        Current >  CurrentTolerance ~ "Charge",
        Current < -CurrentTolerance ~ "Discharge",
        TRUE                       ~ "Rest"
      )
    )
}


# A curve block is one or more consecutive steps with the same current
# direction. For example, a CC charge followed immediately by a CV charge is
# one block. A DCIR pulse following a rest becomes a separate block.
#
# A block is retained when it transfers at least MinCapacity_mAh_g AND at least
# MinFraction of the largest same-direction block in that file/cycle.
#
# Defaults:
#   MinCapacity_mAh_g = 0.10 mAh/g
#   MinFraction       = 0.001 (0.1%)

AddCurveFilterFlags <- function(df,
                                CurrentTolerance = 1e-6,
                                MinCapacity_mAh_g = 0.10,
                                MinFraction = 0.001) {

  RequiredCols <- c(
    "File", "Cycle", "Step", "TestTime", "Current",
    "SpecificChargeCapacity", "SpecificDischargeCapacity"
  )
  MissingCols <- setdiff(RequiredCols, colnames(df))

  if (length(MissingCols) > 0) {
    stop(
      "AddCurveFilterFlags is missing required columns: ",
      paste(MissingCols, collapse = ", ")
    )
  }

  # Record the original row order so steps can be reconstructed even if a
  # schedule reuses a step index later.
  TaggedData <- df |>
    mutate(.OriginalRow = row_number())

  StepSummary <- TaggedData |>
    filter(!is.na(File), !is.na(Cycle), !is.na(Step)) |>
    group_by(File, Cycle, Step) |>
    summarise(
      FirstRow = min(.OriginalRow),
      SignedCurrent = {
        ActiveCurrent <- Current[abs(Current) > CurrentTolerance & !is.na(Current)]
        if (length(ActiveCurrent) == 0) 0 else median(ActiveCurrent)
      },
      ChargeMin = suppressWarnings(min(SpecificChargeCapacity, na.rm = TRUE)),
      ChargeMax = suppressWarnings(max(SpecificChargeCapacity, na.rm = TRUE)),
      DischargeMin = suppressWarnings(min(SpecificDischargeCapacity, na.rm = TRUE)),
      DischargeMax = suppressWarnings(max(SpecificDischargeCapacity, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      across(
        c(ChargeMin, ChargeMax, DischargeMin, DischargeMax),
        ~ if_else(is.infinite(.x), NA_real_, .x)
      ),
      StepDirection = case_when(
        SignedCurrent >  CurrentTolerance ~ "Charge",
        SignedCurrent < -CurrentTolerance ~ "Discharge",
        TRUE                             ~ "Rest"
      )
    ) |>
    arrange(File, Cycle, FirstRow) |>
    group_by(File, Cycle) |>
    mutate(
      # Start a new block whenever direction changes. Rest therefore separates
      # normal cycling from a later diagnostic pulse.
      NewBlock = row_number() == 1L | StepDirection != lag(StepDirection),
      CurveBlock = cumsum(replace_na(NewBlock, TRUE))
    ) |>
    ungroup()

  BlockSummary <- StepSummary |>
    group_by(File, Cycle, CurveBlock, StepDirection) |>
    summarise(
      BlockChargeSpan = max(ChargeMax, na.rm = TRUE) -
                        min(ChargeMin, na.rm = TRUE),
      BlockDischargeSpan = max(DischargeMax, na.rm = TRUE) -
                           min(DischargeMin, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      BlockChargeSpan = if_else(
        is.infinite(BlockChargeSpan), NA_real_, BlockChargeSpan
      ),
      BlockDischargeSpan = if_else(
        is.infinite(BlockDischargeSpan), NA_real_, BlockDischargeSpan
      )
    ) |>
    group_by(File, Cycle) |>
    mutate(
      LargestChargeBlock = suppressWarnings(
        max(BlockChargeSpan[StepDirection == "Charge"], na.rm = TRUE)
      ),
      LargestDischargeBlock = suppressWarnings(
        max(BlockDischargeSpan[StepDirection == "Discharge"], na.rm = TRUE)
      ),
      LargestChargeBlock = if_else(
        is.infinite(LargestChargeBlock), NA_real_, LargestChargeBlock
      ),
      LargestDischargeBlock = if_else(
        is.infinite(LargestDischargeBlock), NA_real_, LargestDischargeBlock
      ),
      ChargeThreshold = pmax(
        MinCapacity_mAh_g,
        MinFraction * LargestChargeBlock,
        na.rm = TRUE
      ),
      DischargeThreshold = pmax(
        MinCapacity_mAh_g,
        MinFraction * LargestDischargeBlock,
        na.rm = TRUE
      ),
      KeepChargeBlock = StepDirection == "Charge" &
                        !is.na(BlockChargeSpan) &
                        BlockChargeSpan >= ChargeThreshold,
      KeepDischargeBlock = StepDirection == "Discharge" &
                           !is.na(BlockDischargeSpan) &
                           BlockDischargeSpan >= DischargeThreshold
    ) |>
    ungroup()

  StepFlags <- StepSummary |>
    select(File, Cycle, Step, CurveBlock, StepDirection) |>
    left_join(
      BlockSummary |>
        select(
          File, Cycle, CurveBlock, StepDirection,
          BlockChargeSpan, BlockDischargeSpan,
          ChargeThreshold, DischargeThreshold,
          KeepChargeBlock, KeepDischargeBlock
        ),
      by = c("File", "Cycle", "CurveBlock", "StepDirection")
    )

  TaggedData |>
    left_join(StepFlags, by = c("File", "Cycle", "Step")) |>
    mutate(
      KeepChargeCurve = replace_na(KeepChargeBlock, FALSE),
      KeepDischargeCurve = replace_na(KeepDischargeBlock, FALSE)
    ) |>
    arrange(.OriginalRow) |>
    select(-.OriginalRow, -KeepChargeBlock, -KeepDischargeBlock)
}


# Optional diagnostic table. Run this after ExtractCycleData() to inspect which
# blocks were retained or rejected before trusting the plots.
CurveFilterSummary <- function(CyclingData) {
  CyclingData |>
    distinct(
      File, Cycle, CurveBlock, StepDirection,
      BlockChargeSpan, BlockDischargeSpan,
      ChargeThreshold, DischargeThreshold,
      KeepChargeCurve, KeepDischargeCurve
    ) |>
    arrange(File, Cycle, CurveBlock)
}