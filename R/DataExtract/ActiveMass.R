# cell_masses.R

ActiveMass <- function() {
  Mass <- c(
    "C1_01" = 0.010266, 
    "C1_02" = NA,
    "C1_03" = NA, 
    "C1_04" = 0.012441,
    "C1_05" = 0.011571,
    "C2_01" = 0.010962, 
    "C2_02" = NA, 
    "C2_03" = NA, 
    "C2_04" = 0.011397, 
    "C2_05" = 0.011658,
    "GA_01" = 0.013311, 
    "GA_02" = NA, 
    "GA_03" = NA, 
    "GA_04" = NA, 
    "GA_05" = NA,
    "GB_01" = 0.016704, 
    "GB_02" = 0.01305, 
    "GB_03" = 0.011397, 
    "GB_04" = NA, 
    "GB_05" = 0.014964,
    "GC_01" = NA, 
    "GC_02" = NA, 
    "GC_03" = NA,
    "Cycle_01_01.xlsx" = 0.009048,
    "Cycle_01_02.xlsx" = 0.012528,
    "Cycle_01_03.xlsx" = 0.009048,
    "Cycle_02_01.xlsx" = 0.011954,
    "Cycle_02_02.xlsx" = 0.013932,
    "Cycle_03_01.xlsx" = 0.01131,
    "Cycle_04_01.xlsx" = 0.013137,
    "Cycle_05_01.xlsx" = 0.011868,
    "Cycle_05_02.xlsx" = 0.011438,
    "Cycle_05_03.xlsx" = NA,
    "Cycle_05_04.xlsx" = NA, # have no clue what this actually is
    "Cycle_08_01.xlsx" = 0.011266,
    "Cycle_08_02.xlsx" = 0.014018,
    "Cycle_09_01.xlsx" = 0.01333,
    "Cycle_09_02.xlsx" = 0.012298,
    "Cycle_03_01_Ch30.xlsx" = 0.011314,
    
    "LMRO_Cycle_1.xlsx" = 0.014603,
    "LMRO_Cycle_2.xlsx" = 0.015807,
    "LMRO_Cycle_3.xlsx" = 0.015893,
    "LMRO_Cycle_4.xlsx" = 0.0157208,
    "LMRO_Cycle_5.xlsx" = 0.0164088,
    "LMRO_Cycle_6.xlsx" = 0.0154628,
    "LMRO_Cycle_7.xlsx" = 0.0169248,
    "LMRO_Cycle_8.xlsx" = 0.00602,
    "LMRO_Cycle_9.xlsx" = 0.0144308,
    "LMRO_Cycle_16.xlsx" = 0.01348,
    "LMRO_Cycle_16_RateTesting.xlsx" = 0.01348,
    "LMRO_Cycle_17.xlsx" = 0.01788,
    "LMRO_Cycle_18.xlsx" = 0.01625,
    "LMRO_Cycle_21.xlsx" = 0.01554,
    "LMRO_Cycle_22.xlsx" = 0.01417,
    "LMRO_Cycle_23.xlsx" = 0.01606,
    "LMRO_Cycle_24.xlsx" = 0.014517,
    "LMRO_Cycle_25.xlsx" = 0.01503,

    "LMRO_Cycle_26.xlsx" = 0.01572,
    "LMRO_Cycle_27.xlsx" = 0.0230,
    "LMRO_Cycle_28.xlsx" = 0.02308,
    "LMRO_Cycle_29.xlsx" = 0.01208,
    "LMRO_Cycle_30.xlsx" = 0.0108,
    "LMRO_Cycle_31.xlsx" = 0.00928,
    "LMRO_Cycle_32.xlsx" = 0.00792,
    "LMRO_Cycle_36.xlsx" = 0.00928,
    "LMRO_Cycle_37.xlsx" = 0.0108,



    "811_Cycle_11.xlsx" = 0.01953,
    "811_Cycle_12.xlsx" = 0.02025,  
    "811_Cycle_13.xlsx" = 0.01818,
    "811_Cycle_14.xlsx" = 0.01926,
    "811_Cycle_15.xlsx" = 0.01935,
    "811_Cycle_19.xlsx" = 0.02052,
    "811_Cycle_20.xlsx" = 0.01953,
    "811_Cycle_33.xlsx" = 0.0252,
    "811_Cycle_34.xlsx" = 0.02079,
    "811_Cycle_35.xlsx" = 0.02034,

    "DOE_Cycle_2_1.xlsx" = 0.0258,
    "DOE_Cycle_2_2.xlsx" = 0.0255,
    "DOE_Cycle_2_3.xlsx" = 0.0178,
    "DOE_Cycle_2_4.xlsx" = 0.0257,
    "DOE_Cycle_2_5.xlsx" = 0.0257,





    "Alex_Cycle_1.xlsx" = 0.0201,
    "Alex_Cycle_2.xlsx" = 0.0201,

    "AR_LMO_Cycle_6.xlsx" = 0.0169,
    "AR_LMO_Cycle_7.xlsx" = 0.0191,
    "AR_LMO_Cycle_8.xlsx" = 0.0175,
    "AR_LMO_Cycle_10.xlsx" = 0.0150,
    "AR_LMO_Cycle_11.xlsx" = 0.0169,
    "AR_LMO_Cycle_12.xlsx" = 0.0191,
    "AR_LMO_Cycle_13.xlsx" = 0.0175,
    "AR_LMO_Cycle_14.xlsx" = 0.0138,
    "AR_LMO_Cycle_15.xlsx" = 0.0136,
    "AR_LMO_Cycle_16.xlsx" = 0.0131,

    "MC_LMRO_1_Cycle.xlsx" = 0.0169,
    "MC_LMRO_2_Cycle.xlsx" = 0.0191,
    "MC_LMRO_3_Cycle.xlsx" = 0.0175,
    "MC_LMRO_4_Cycle.xlsx" = 0.0198,
    "MC_LMRO_7_Cycle.xlsx" = 0.0150,
    "MC_LMRO_8_Cycle.xlsx" = 0.0143


  )
  return(Mass)
}

# Safe lookup wrapper — warns if mass is missing for a given filename
GetActiveMass <- function(filename) {
  Masses <- ActiveMass()
  mass   <- Masses[filename]
  
  if (is.na(mass)) {
    warning("No active mass recorded for: ", filename, " — specific capacity will be NA")
  }
  
  return(mass)
}