# =============================================================================
# LabelSteps.R
# Labels each row as Charge, Discharge, or Rest based on current sign
# =============================================================================

LabelSteps <- function(df) {
  df |>
    mutate(
      StepType = case_when(
        Current >  1e-6  ~ "Charge",
        Current < -1e-6  ~ "Discharge",
        TRUE             ~ "Rest"
      )
    )
}