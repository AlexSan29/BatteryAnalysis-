# Load all required libraries
library(readxl)       # read Arbin .xlsx files
library(writexl)      # save stats output to .xlsx
library(patchwork)    # combine plots
library(scales)       # axis label formatting
library(directlabels) # direct plot labels
library(ggbeeswarm)   # beeswarm plot geometry
library(signal)       # Savitzky-Golay filter for dQ/dV smoothing
library(tidyverse)    # includes dplyr, ggplot2, purrr, stringr, lubridate
library(dplyr)
filter <- dplyr::filter