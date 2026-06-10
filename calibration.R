# CALIBRATION QUALITY CHECK
# Extracts calibration and validation accuracy per participant from the Tobii export to varify data quality.

#Install packages, only run it the first time
#install.packages ("dplyr")
#install.packages ("rstudioapi")
#install.packages ("readxl")
library(writexl)
library (rstudioapi)
library (dplyr)

# Set working directory 
setwd (dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Read the columns of the tsv
data <- read.delim ("questions_export.tsv")
names (data)

# Extract one row per participant withtheir calibration metrics: calibration accuracy, calibration precision, validation accuracy, validation precision.
calibration_summary <- data %>%
  group_by(Participant.name) %>%
  slice(1) %>%  # take first row per participant
  ungroup() %>%
  select(Participant.name,
         Average.calibration.accuracy..degrees.,
         Average.calibration.precision.SD..degrees.,
         Average.validation.accuracy..degrees.,
         Average.validation.precision.SD..degrees.)

View(calibration_summary)

calibration_by_participant <- data %>%
  group_by(Participant.name) %>%
  slice(1) %>%
  ungroup() %>%
  select(Participant.name,
         Average.calibration.accuracy..degrees.,
         Average.calibration.precision.SD..degrees.,
         Average.validation.accuracy..degrees.,
         Average.validation.precision.SD..degrees.)

# Export excel for reporting
write_xlsx(calibration_by_participant, "calibration_by_participant.xlsx")




# Overall summary across all participants
calibration_summary %>%
  summarise(
    mean_cal_accuracy = round(mean(Average.calibration.accuracy..degrees., na.rm = TRUE), 2),
    max_cal_accuracy  = round(max(Average.calibration.accuracy..degrees., na.rm = TRUE), 2),
    mean_val_accuracy = round(mean(Average.validation.accuracy..degrees., na.rm = TRUE), 2),
    max_val_accuracy  = round(max(Average.validation.accuracy..degrees., na.rm = TRUE), 2)
  )