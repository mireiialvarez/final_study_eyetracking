# R Code for the Master's Thesis Eye-Tracking Study (Catalan - English)

This repository contains the R scripts used for the analyses reported in the Master's thesis.

## First-pass Duration and Total Fixation Duration

To calculate the metrics of **first-pass duration** and **total fixation duration**, see:

- `firstpassduration_totalduration.R`

To run this script, you will need:

- `firstpassdur_totaldur.tsv`  
  (file extracted from the Tobii Pro Lab metrics)

## Regressions (Selective Path Regression and First-pass Regression)

To calculate **regressions** (*selective path regression* and *first-pass regression*), see:

- `regressions.R`

To run this script, you will need:

- `data_merge1.parquet`  
  (containing PP raw data)
- `data_merge2.parquet`  
  (containing RC raw data)
- `tobii_metrics.xlsx`  
  (Excel file generated from the first-pass duration and total duration analyses, used to extract the regions for each word and sentence)

## Comprehension Questions

To compile all data from the **comprehension questions**, see:

- `questions.R`

To run this script, you will need:

- `questions_export.tsv`  
  (file containing Keyboard Events used to retrieve participants’ responses)
