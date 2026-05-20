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

- `pp_long_filtred.parquet`  
  (containing PP raw data)
- `rc_long_filtred.parquet`  
  (containing RC raw data)
- `tobii_metrics.xlsx`  
  (Excel file generated from the first-pass duration and total duration analyses, used to extract the regions for each word and sentence)

**Note:** The original raw data files (`data_merge1.parquet` and `data_merge2.parquet`) were not uploaded due to file size limitations. The provided filtered `.parquet` files are lighter processed versions and allow the analyses to be reproduced starting from a later stage of the pipeline.

## Comprehension Questions

To compile all data from the **comprehension questions**, see:

- `questions.R`

This script requires:

- `questions_export.tsv`  
  (file containing Keyboard Events used to retrieve participants’ responses)

**Note:** The original `questions_export.tsv` file is not included in this repository due to file size limitations. The script can be reproduced using a file with the same structure exported from Tobii Pro Lab Keyboard Event

## Explanatory Analysis

To calculate and compile the data of the **explanatory analysis** for object and subject RCs, see:
- `explanatory_analysis.R`

This script requires:
- `pp_long_filtred.parquet`  
  (containing PP raw data)
- `rc_long_filtred.parquet`  
  (containing RC raw data)
- `tobii_metrics.xlsx`  
  (Excel file generated from the first-pass duration and total duration analyses, used to extract the regions for each word and sentence)
- `ambiguous_responses.xlsx`
- `control_accuracy_appendix_summary.xlsx`
