# ANALYSIS OF EYE-TRACKING DATA 2
# Measures: Selective Regression Path and First Pass Regression
# Languages: Catalan and English
#Set working directory to the script's location
setwd (dirname(rstudioapi::getActiveDocumentContext()$path))
#Install packages, only run it the first time
#install.packages("arrow") 
#install.packages ("dplyr")
#install.packages ("tidyr")
#install.packages ("readxl")
#install.packages ("stringr")
#install.packages ("duckdb")
#install.packages ("rstudioapi")
#install.packages("ggplot2")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("extrafont")
#install.packages("patchwork")
library(arrow)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(duckdb)
library (rstudioapi)
library (ggplot2)
library(lme4)
library(lmerTest)
library(extrafont)
library(patchwork)

# STEP 1: MERGE PARQUET FILES + SCHEMA CHECK
# Create the working folder and a temp folder for DuckDB
dir.create("C:/Users/clic/Documents/mireia tfm/finalstudy, showWarnings = FALSE)
dir.create("C:/duckdb_tmp", showWarnings = FALSE)

# Close any leftover database connection from previous run
try(dbDisconnect(con), silent = TRUE)

#Open a new DuckDB connection and configure it for a low-memory machine
con <- dbConnect(duckdb())
dbExecute(con, "SET memory_limit='8GB'")
dbExecute(con, "SET temp_directory='C:/duckdb_tmp'")
dbExecute(con, "SET threads=2")
dbExecute(con, "SET preserve_insertion_order=false")

#Merge the two parquet files into one, matching columns by name
cat("Merging final...\n")
dbExecute(con, "
  COPY (
    SELECT * FROM read_parquet('data_merge1.parquet')
    UNION ALL BY NAME
    SELECT * FROM read_parquet('data_merge2.parquet')
  )
  TO 'data_full.parquet' (FORMAT PARQUET, COMPRESSION 'SNAPPY')
  ")
cat("Done! data_full.parquet ready.\n")

# Close and reopen the connection (clears DuckDB's internal state)
dbDisconnect(con)
try(dbDisconnect(con), silent = TRUE)
con <- dbConnect(duckdb())

#Post-hoc verification
# Check schemas of both merge files
s1 <- dbGetQuery(con, "SELECT name, type FROM parquet_schema('data_merge1.parquet')")
s2 <- dbGetQuery(con, "SELECT name, type FROM parquet_schema('data_merge2.parquet')")

# Find columns that differ between the two
diff1 <- s1[!s1$name %in% s2$name, ]  # in merge1 but not merge2
diff2 <- s2[!s2$name %in% s1$name, ]  # in merge2 but not merge1

cat("Columns only in merge1:\n"); print(diff1)
cat("Columns only in merge2:\n"); print(diff2)

# Also check for same name but different type
common <- merge(s1, s2, by="name", suffixes=c("_m1","_m2"))
type_conflicts <- common[common$type_m1 != common$type_m2, ]
cat("Type conflicts:\n"); print(type_conflicts)

dbDisconnect(con)

#Set wd
setwd ("C:/Users/clic/Documents/mireia tfm/finalstudy")
 
  # Find all AOI hit columns from the parquet file
  cat("Getting AOI hit columns for", parquet_file, "...\n")
  schema <- dbGetQuery(con, sprintf("SELECT name FROM parquet_schema('%s')", parquet_file))
  aoi_cols <- schema$name[grepl("AOI hit", schema$name)]
  cat("Found", length(aoi_cols), "AOI hit columns\n")
 
  # For each AOI column, build a SELECT query that:
  # keeps only fixation rows (ignores blinks and saccades)
  # keeps only rows where the participant looked at that AOI (NA=0 and then delete rows with '0')
  # extracts the AOI name as a value ("word1") instead of a column header.
  # keeps Fixation point X and Y coordinates to later disambiguate repeated words
  # keeps participants name, fixation index and duration
  cat("Building query for", parquet_file, "...\n")
 
  union_parts <- sapply(aoi_cols, function(col) {
    # Clean AOI name: remove "AOI hit [" and "]"
    clean_name <- gsub("AOI hit \\[|\\]", "", col)
    clean_name <- gsub("'", "''", clean_name)  # escape single quotes
    sprintf(
      "SELECT \"Participant name\", \"Eye movement type\", \"Eye movement event duration\", \"Eye movement type index\", \"Fixation point X\", \"Fixation point Y\", '%s' AS AOI, CAST(COALESCE(\"%s\", '0') AS INTEGER) AS hit FROM read_parquet('%s') WHERE \"Eye movement type\" = 'Fixation' AND COALESCE(\"%s\", '0') <> '0'",
  clean_name, col, parquet_file, col
)
})

# Combine all those SELECT queries with UNION ALL
full_sql <- paste(union_parts, collapse = "\n UNION ALL \n")

# Run the combined query and save result as a temporary DuckDB table
cat("Creating long table...\n")
dbExecute(con, sprintf("CREATE OR REPLACE TABLE %s_raw AS %s", output_name, full_sql))

# Deduplicate: if same participant + AOI + fixation index appears more than once,
# keep only the first duration value
cat("Deduplicating...\n")
dbExecute(con, sprintf("
    CREATE OR REPLACE TABLE %s_clean AS
    SELECT
      \"Participant name\",
      \"Eye movement type\",
      \"Eye movement type index\",
      \"Fixation point X\",
      \"Fixation point Y\",
      AOI,
      FIRST(\"Eye movement event duration\") AS duration
    FROM %s_raw
    GROUP BY
      \"Participant name\",
      \"Eye movement type\",
      \"Eye movement type index\",
      \"Fixation point X\",
      \"Fixation point Y\",
      AOI
  ", output_name, output_name))

# Save it as a small parquet file
dbExecute(con, sprintf("
    COPY (SELECT * FROM %s_clean)
    TO '%s_long.parquet'
    (FORMAT PARQUET, COMPRESSION 'SNAPPY')
  ", output_name, output_name))

# Print row count so we can verify it worked
n <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM %s_clean", output_name))
cat("Done!", paste0(output_name, "_long.parquet"), "- Rows:", n$n, "\n\n")
}

# Connect to DuckDB and set memory options
con <- dbConnect(duckdb())
dbExecute(con, "SET memory_limit='8GB'")
dbExecute(con, "SET temp_directory='C:/duckdb_tmp'")
dbExecute(con, "SET threads=2")

#STEP 2: WIDE-TO-LONG RESHAPE
# Run the function on both merge files
# data_merge1 = prepositional phrase in cat and eng / data_merge2 = relative clause in cat and eng
process_file(con, "data_merge1.parquet", "pp")
process_file(con, "data_merge2.parquet", "rc")

dbDisconnect(con)

# STEP 3: FILTER FIXATION DURATIONS 
setwd("C:/Users/clic/Documents/mireia tfm/finalstudy")
con <- dbConnect(duckdb())

# Filter out fixations shorter than 80ms or longer than 1200ms

for (cond in c("pp", "rc")) {
  cat("Filtering fixation durations for", cond, "...\n")
  
  dbExecute(con, sprintf("
    COPY (
      SELECT *
      FROM read_parquet('%s_long.parquet')
      WHERE CAST(duration AS INTEGER) >= 80
        AND CAST(duration AS INTEGER) <= 1200
    )
    TO '%s_long_filtered.parquet'
    (FORMAT PARQUET, COMPRESSION 'SNAPPY')
  ", cond, cond))
  
  # Print before/after counts to report % of exclusion fixations
  n_before <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM read_parquet('%s_long.parquet')", cond))
  n_after  <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM read_parquet('%s_long_filtered.parquet')", cond))
  cat(cond, "- Before:", n_before$n, "| After:", n_after$n,
      "| Excluded:", n_before$n - n_after$n, "\n\n")
}

dbDisconnect(con)

# STEP 4: FIX NAMING INCONSISTENCIES

con <- dbConnect(duckdb())

# Write to a temporary file first
# Fix 1: remove (1) from ctrrc135_cat and rc135_cat
# Fix 2: rename ctrrc_30eng to ctrrc30_eng
dbExecute(con, "
  COPY (
    SELECT
      \"Participant name\",
      \"Eye movement type\",
      \"Eye movement type index\",
      \"Fixation point X\",
      \"Fixation point Y\",
      regexp_replace(
        regexp_replace(AOI, '(ctrrc135_cat|rc135_cat)\\s*\\(1\\)', '\\1'),
        'ctrrc_30eng', 'ctrrc30_eng'
      ) AS AOI,
      duration
    FROM read_parquet('rc_long_filtered.parquet')
  )
  TO 'rc_long_filtered_temp.parquet'
  (FORMAT PARQUET, COMPRESSION 'SNAPPY')
")

dbDisconnect(con)

# Rename: delete original and rename temp to original
file.remove("rc_long_filtered.parquet")
file.rename("rc_long_filtered_temp.parquet", "rc_long_filtered.parquet")

# Verify both fixes
con <- dbConnect(duckdb())
cat("=== rc135_cat check ===\n")
print(dbGetQuery(con, "
  SELECT DISTINCT AOI
  FROM read_parquet('rc_long_filtered.parquet')
  WHERE AOI LIKE '%rc135_cat%'
"))

cat("=== ctrrc30_eng check ===\n")
print(dbGetQuery(con, "
  SELECT DISTINCT AOI
  FROM read_parquet('rc_long_filtered.parquet')
  WHERE AOI LIKE '%ctrrc30%' OR AOI LIKE '%ctrrc_30%'
"))
dbDisconnect(con)

# STEP 5: MERGE PARQUET FILES WITH EXCEL REGIONS
setwd("C:/Users/clic/Documents/mireia tfm/finalstudy")

# Load Excel
regions <- read_excel("tobii_metrics.xlsx")
regions <- unique(regions[, c("TOI", "AOI", "Region")])
regions$AOI_key <- paste0(regions$TOI, " - ", regions$AOI)

# Fix all multiple spaces in lookup and build ordered lookup
region_lookup_ordered <- regions %>%
  mutate(AOI_key = gsub("\\s+", " ", AOI_key)) %>%
  group_by(AOI_key) %>%
  arrange(Region) %>%
  mutate(occurrence = row_number()) %>%
  ungroup()

# Load filtered parquet files
pp <- read_parquet("pp_long_filtered.parquet")
rc <- read_parquet("rc_long_filtered.parquet")

# Merge regions function
merge_regions <- function(df) {
  df %>%
    mutate(
      AOI = gsub("\\s+", " ", AOI),  # fix any number of spaces
      AOI_base = sub("_(\\d+)$", "", AOI),
      suffix_num = ifelse(grepl("_(\\d+)$", AOI),
                          as.integer(regmatches(AOI, regexpr("(?<=_)\\d+$", AOI, perl=TRUE))),
                          0),
      occurrence = suffix_num + 1
    ) %>%
    # Try 1: exact match with occurrence
    left_join(region_lookup_ordered[, c("AOI_key", "occurrence", "Region")],
              by = c("AOI_base" = "AOI_key", "occurrence" = "occurrence")) %>%
    # Try 2: fallback to occurrence 1 (re-fixations and capital letter words)
    left_join(region_lookup_ordered %>%
                filter(occurrence == 1) %>%
                select(AOI_key, Region) %>%
                rename(Region_fallback = Region),
              by = c("AOI_base" = "AOI_key")) %>%
    mutate(Region = ifelse(is.na(Region), Region_fallback, Region)) %>%
    select(-AOI_base, -suffix_num, -occurrence, -Region_fallback)
}

pp_regions <- merge_regions(pp)
rc_regions <- merge_regions(rc)

# Manual fix for ctrrc22_eng - triple spaces caused matching failure
# Sentence: "The photographers of the model who was very famous arrived extremely late."
rc_regions <- rc_regions %>%
  mutate(Region = case_when(
    gsub("\\s+", " ", AOI) %in% c("ctrrc22_eng - The",
                                  "ctrrc22_eng - photographers") & is.na(Region) ~ "r1",
    gsub("\\s+", " ", AOI) %in% c("ctrrc22_eng - of",
                                  "ctrrc22_eng - the",
                                  "ctrrc22_eng - the_1",
                                  "ctrrc22_eng - model") & is.na(Region) ~ "r2",
    gsub("\\s+", " ", AOI) %in% c("ctrrc22_eng - who",
                                  "ctrrc22_eng - was",
                                  "ctrrc22_eng - very",
                                  "ctrrc22_eng - famous") & is.na(Region) ~ "r3",
    gsub("\\s+", " ", AOI) %in% c("ctrrc22_eng - arrived",
                                  "ctrrc22_eng - extremely",
                                  "ctrrc22_eng - late.") & is.na(Region) ~ "r4",
    TRUE ~ Region
  ))

cat("PP - NAs in Region:", sum(is.na(pp_regions$Region)), "\n")
cat("RC - NAs in Region:", sum(is.na(rc_regions$Region)), "\n")

View(pp_regions)
View(rc_regions)

# Extract sentence identifier to have them well classified for both pp and rc. 

pp_regions <- pp_regions %>%
  mutate(
    text_id = sub(" - .*", "", AOI),  # extract sentence ID from AOI name
    Condition = ifelse(str_detect(text_id, "ctr"), "Control", "Ambiguous"),
    Language = case_when(
      str_detect(text_id, "cat") ~ "Catalan",
      str_detect(text_id, "eng") ~ "English",
      TRUE ~ "Other"
    ),
    Type = "PP"  # all pp_regions are PP
  )

rc_regions <- rc_regions %>%
  mutate(
    text_id = sub(" - .*", "", AOI),  # extract sentence ID from AOI name
    Condition = ifelse(str_detect(text_id, "ctr"), "Control", "Ambiguous"),
    Language = case_when(
      str_detect(text_id, "cat") ~ "Catalan",
      str_detect(text_id, "eng") ~ "English",
      TRUE ~ "Other"
    ),
    Type = "RC"  # all rc_regions are RC
  )

# Quick check
pp_regions %>% count(Condition, Language, Type)
rc_regions %>% count(Condition, Language, Type)
    
  )

View (pp_regions)

# STEP 6: DESCRIPTIVE PLOTS 
# Calculating Selective path regression (spr) for R3 
# spr: sum of fixation durations from first fixation on r3 until just before first fixation on r4

spr_pp_r3 <- pp_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type) %>%
  arrange(`Eye movement type index`) %>%
  mutate(
    # spr for the pp file
    # First fixation index on r3
    first_r3_index = ifelse(
      any(Region == "r3", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r3"], na.rm = TRUE),
      NA
    ),
    # First fixation index on r4
    first_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      max(`Eye movement type index`, na.rm = TRUE)
    ),
    # Window: from first r3 until just before r4
    in_window = `Eye movement type index` >= first_r3_index &
      `Eye movement type index` < first_r4_index
  ) %>%
  filter(in_window) %>%
  summarise(
    selective_path_regression = sum(duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()
# Do the same for the rc file 
spr_rc_r3 <- rc_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type) %>%
  arrange(`Eye movement type index`) %>%
  mutate(
    first_r3_index = ifelse(
      any(Region == "r3", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r3"], na.rm = TRUE),
      NA
    ),
    first_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      max(`Eye movement type index`, na.rm = TRUE)
    ),
    in_window = `Eye movement type index` >= first_r3_index &
      `Eye movement type index` < first_r4_index
  ) %>%
  filter(in_window) %>%
  summarise(
    selective_path_regression = sum(duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

View(spr_pp_r3)
View(spr_rc_r3)

# Calculating Selective path regression (spr) for R4
# For pp 
spr_pp_r4 <- pp_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type) %>%
  arrange(`Eye movement type index`) %>%
  mutate(
    # First fixation index on r4
    first_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      NA
    ),
    # Last fixation index on r4
    last_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      max(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      NA
    ),
    # Window: from first r4 to last r4
    in_window = `Eye movement type index` >= first_r4_index &
      `Eye movement type index` <= last_r4_index
  ) %>%
  filter(in_window) %>%
  summarise(
    selective_path_regression_r4 = sum(duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()
# For rc
spr_rc_r4 <- rc_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type) %>%
  arrange(`Eye movement type index`) %>%
  mutate(
    first_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      NA
    ),
    last_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      max(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      NA
    ),
    in_window = `Eye movement type index` >= first_r4_index &
      `Eye movement type index` <= last_r4_index
  ) %>%
  filter(in_window) %>%
  summarise(
    selective_path_regression_r4 = sum(duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

View(spr_pp_r4)
View(spr_rc_r4)

# First pass regression for r3
# 1 = eye went back to r1 or r2 after first entering r3 (before reaching r4)
# 0 = eye moved forward from r3 to r4 without regressing
# For pp 
fpr_pp_r3 <- pp_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type) %>%
  arrange(`Eye movement type index`) %>%
  mutate(
    first_r3_index = ifelse(
      any(Region == "r3", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r3"], na.rm = TRUE),
      NA
    ),
    first_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      max(`Eye movement type index`, na.rm = TRUE)
    )
  ) %>%
  summarise(
    firstpass_regression_r3 = ifelse(
      is.na(first(first_r3_index)),
      NA,
      as.integer(any(
        `Eye movement type index` > first(first_r3_index) &
          `Eye movement type index` < first(first_r4_index) &
          Region %in% c("r1", "r2")
      ))
    ),
    .groups = "drop"
  ) %>%
  ungroup()
# Same process but for rc
fpr_rc_r3 <- rc_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type) %>%
  arrange(`Eye movement type index`) %>%
  mutate(
    first_r3_index = ifelse(
      any(Region == "r3", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r3"], na.rm = TRUE),
      NA
    ),
    first_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      max(`Eye movement type index`, na.rm = TRUE)
    )
  ) %>%
  summarise(
    firstpass_regression_r3 = ifelse(
      is.na(first(first_r3_index)),
      NA,
      as.integer(any(
        `Eye movement type index` > first(first_r3_index) &
          `Eye movement type index` < first(first_r4_index) &
          Region %in% c("r1", "r2")
      ))
    ),
    .groups = "drop"
  ) %>%
  ungroup()

View(fpr_pp_r3)
View(fpr_rc_r3)

# First pass regression for r4
# 1 = eye went back to r1, r2 or r3 after first entering r4
# 0 = eye did not regress after entering r4
# For pp 
fpr_pp_r4 <- pp_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type) %>%
  arrange(`Eye movement type index`) %>%
  mutate(
    first_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      NA
    ),
    last_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      max(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      NA
    )
  ) %>%
  summarise(
    firstpass_regression_r4 = ifelse(
      is.na(first(first_r4_index)),
      NA,
      as.integer(any(
        `Eye movement type index` > first(first_r4_index) &
          `Eye movement type index` <= first(last_r4_index) &
          Region %in% c("r1", "r2", "r3")
      ))
    ),
    .groups = "drop"
  ) %>%
  ungroup()
# Same process but for rc
fpr_rc_r4 <- rc_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type) %>%
  arrange(`Eye movement type index`) %>%
  mutate(
    first_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      min(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      NA
    ),
    last_r4_index = ifelse(
      any(Region == "r4", na.rm = TRUE),
      max(`Eye movement type index`[Region == "r4"], na.rm = TRUE),
      NA
    )
  ) %>%
  summarise(
    firstpass_regression_r4 = ifelse(
      is.na(first(first_r4_index)),
      NA,
      as.integer(any(
        `Eye movement type index` > first(first_r4_index) &
          `Eye movement type index` <= first(last_r4_index) &
          Region %in% c("r1", "r2", "r3")
      ))
    ),
    .groups = "drop"
  ) %>%
  ungroup()

View(fpr_pp_r4)
View(fpr_rc_r4)

# Count NAs for each measure

cat("=== FIRST PASS REGRESSION ===\n")
cat("PP r3 - NA:", sum(is.na(fpr_pp_r3$firstpass_regression_r3)),
    "| Valid:", sum(!is.na(fpr_pp_r3$firstpass_regression_r3)), "\n")
cat("RC r3 - NA:", sum(is.na(fpr_rc_r3$firstpass_regression_r3)),
    "| Valid:", sum(!is.na(fpr_rc_r3$firstpass_regression_r3)), "\n")
cat("PP r4 - NA:", sum(is.na(fpr_pp_r4$firstpass_regression_r4)),
    "| Valid:", sum(!is.na(fpr_pp_r4$firstpass_regression_r4)), "\n")
cat("RC r4 - NA:", sum(is.na(fpr_rc_r4$firstpass_regression_r4)),
    "| Valid:", sum(!is.na(fpr_rc_r4$firstpass_regression_r4)), "\n")

cat("\n=== SELECTIVE PATH REGRESSION ===\n")
cat("PP r3 - NA:", sum(is.na(spr_pp_r3$selective_path_regression)),
    "| Valid:", sum(!is.na(spr_pp_r3$selective_path_regression)), "\n")
cat("RC r3 - NA:", sum(is.na(spr_rc_r3$selective_path_regression)),
    "| Valid:", sum(!is.na(spr_rc_r3$selective_path_regression)), "\n")
cat("PP r4 - NA:", sum(is.na(spr_pp_r4$selective_path_regression_r4)),
    "| Valid:", sum(!is.na(spr_pp_r4$selective_path_regression_r4)), "\n")
cat("RC r4 - NA:", sum(is.na(spr_rc_r4$selective_path_regression_r4)),
    "| Valid:", sum(!is.na(spr_rc_r4$selective_path_regression_r4)), "\n")

# Combine pp and rc for each measure
spr_r3_combined <- bind_rows(
  spr_pp_r3 %>% mutate(Structure = "PP"),
  spr_rc_r3 %>% mutate(Structure = "RC")
)

spr_r4_combined <- bind_rows(
  spr_pp_r4 %>% mutate(Structure = "PP"),
  spr_rc_r4 %>% mutate(Structure = "RC")
)

fpr_r3_combined <- bind_rows(
  fpr_pp_r3 %>% mutate(Structure = "PP"),
  fpr_rc_r3 %>% mutate(Structure = "RC")
)

fpr_r4_combined <- bind_rows(
  fpr_pp_r4 %>% mutate(Structure = "PP"),
  fpr_rc_r4 %>% mutate(Structure = "RC")
)

# Check
spr_r3_combined %>% count(Condition, Language, Structure)

# Descriptive statistics: Graphics with raw data. 
# First import all fonts from your system
font_import(prompt = FALSE)  # prompt = FALSE skips the confirmation
# Load fonts
loadfonts(device = "win")
# Define colours
condition_colors <- c("Control" = "steelblue", "Ambiguous" = "firebrick")

# make_plot function with Times New Roman and grid
make_plot <- function(data, y_var, se_var, title, y_label, y_limits) {
  ggplot(data, aes(x = Structure, y = .data[[y_var]], fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
    geom_errorbar(aes(ymin = .data[[y_var]] - .data[[se_var]],
                      ymax = .data[[y_var]] + .data[[se_var]]),
                  position = position_dodge(0.8), width = 0.25) +
    scale_fill_manual(values = condition_colors) +
    scale_y_continuous(limits = y_limits) +
    labs(title = title, x = "Structure Type", y = y_label) +
    theme(
      text = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      plot.title = element_text(size = 10, hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.position = "right",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.minor = element_blank()
    )
}

# Calculate summaries for each measure
calc_summary <- function(data, measure_col) {
  data %>%
    filter(!is.na(.data[[measure_col]])) %>%
    group_by(Condition, Language, Structure) %>%
    summarise(
      mean_val = mean(.data[[measure_col]], na.rm = TRUE),
      se_val = sd(.data[[measure_col]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
}

# Figure 1: Selective Path Regression R3 
spr_r3_summary <- calc_summary(spr_r3_combined, "selective_path_regression")
y_max_spr_r3 <- max(spr_r3_summary$mean_val + spr_r3_summary$se_val) * 1.1

p_spr_r3_cat <- make_plot(
  filter(spr_r3_summary, Language == "Catalan"),
  "mean_val", "se_val", "Catalan",
  "Mean Selective Path Regression (ms)", c(0, y_max_spr_r3)
)
p_spr_r3_eng <- make_plot(
  filter(spr_r3_summary, Language == "English"),
  "mean_val", "se_val", "English",
  "Mean Selective Path Regression (ms)", c(0, y_max_spr_r3)
)

figure_spr_r3 <- (p_spr_r3_cat | p_spr_r3_eng) +
  plot_annotation(
    caption = "Figure X. Mean selective path regression (ms) in R3 for PP and RC structures\nin Catalan (left) and English (right). Error bars represent standard error.\nBlue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("Figure_SPR_R3.png", figure_spr_r3, width = 18, height = 10, units = "cm", dpi = 300)

# Figure 2: Selective Path Regression R4 
spr_r4_summary <- calc_summary(spr_r4_combined, "selective_path_regression_r4")
y_max_spr_r4 <- max(spr_r4_summary$mean_val + spr_r4_summary$se_val) * 1.1

p_spr_r4_cat <- make_plot(
  filter(spr_r4_summary, Language == "Catalan"),
  "mean_val", "se_val", "Catalan",
  "Mean Selective Path Regression (ms)", c(0, y_max_spr_r4)
)
p_spr_r4_eng <- make_plot(
  filter(spr_r4_summary, Language == "English"),
  "mean_val", "se_val", "English",
  "Mean Selective Path Regression (ms)", c(0, y_max_spr_r4)
)

figure_spr_r4 <- (p_spr_r4_cat | p_spr_r4_eng) +
  plot_annotation(
    caption = "Figure X. Mean selective path regression (ms) in R4 for PP and RC structures\nin Catalan (left) and English (right). Error bars represent standard error.\nBlue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("Figure_SPR_R4.png", figure_spr_r4, width = 18, height = 10, units = "cm", dpi = 300)

# Figure 3: First Pass Regression R3 (%) 
fpr_r3_combined <- fpr_r3_combined %>%
  mutate(firstpass_pct = firstpass_regression_r3 * 100)

fpr_r3_summary <- calc_summary(fpr_r3_combined, "firstpass_pct")
y_max_fpr_r3 <- max(fpr_r3_summary$mean_val + fpr_r3_summary$se_val) * 1.1

p_fpr_r3_cat <- make_plot(
  filter(fpr_r3_summary, Language == "Catalan"),
  "mean_val", "se_val", "Catalan",
  "First-pass Regression (%)", c(0, y_max_fpr_r3)
)
p_fpr_r3_eng <- make_plot(
  filter(fpr_r3_summary, Language == "English"),
  "mean_val", "se_val", "English",
  "First-pass Regression (%)", c(0, y_max_fpr_r3)
)

figure_fpr_r3 <- (p_fpr_r3_cat | p_fpr_r3_eng) +
  plot_annotation(
    caption = "Figure X. First-pass regression (%) in R3 for PP and RC structures\nin Catalan (left) and English (right). Error bars represent standard error.\nBlue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("Figure_FPR_R3.png", figure_fpr_r3, width = 18, height = 10, units = "cm", dpi = 300)

# Figure 4: First Pass Regression R4 (%) 
fpr_r4_combined <- fpr_r4_combined %>%
  mutate(firstpass_pct = firstpass_regression_r4 * 100)

fpr_r4_summary <- calc_summary(fpr_r4_combined, "firstpass_pct")
y_max_fpr_r4 <- max(fpr_r4_summary$mean_val + fpr_r4_summary$se_val) * 1.1

p_fpr_r4_cat <- make_plot(
  filter(fpr_r4_summary, Language == "Catalan"),
  "mean_val", "se_val", "Catalan",
  "First-pass Regression (%)", c(0, y_max_fpr_r4)
)
p_fpr_r4_eng <- make_plot(
  filter(fpr_r4_summary, Language == "English"),
  "mean_val", "se_val", "English",
  "First-pass Regression (%)", c(0, y_max_fpr_r4)
)

figure_fpr_r4 <- (p_fpr_r4_cat | p_fpr_r4_eng) +
  plot_annotation(
    caption = "Figure X. First-pass regression (%) in R4 for PP and RC structures\nin Catalan (left) and English (right). Error bars represent standard error.\nBlue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("Figure_FPR_R4.png", figure_fpr_r4, width = 18, height = 10, units = "cm", dpi = 300)

cat("All 4 figures saved!\n")

#Checking  
fpr_r4_combined %>%
  filter(!is.na(firstpass_pct)) %>%
  group_by(Condition, Language, Structure) %>%
  summarise(mean = mean(firstpass_pct), .groups = "drop")

pp_regions %>%
  filter(`Participant name` == "Participant1") %>%
  filter(grepl("pp111_cat", AOI)) %>%
  arrange(`Eye movement type index`) %>%
  select(`Eye movement type index`, AOI, Region, duration)
pp_regions %>%
  filter(`Participant name` == "Participant1") %>%
  filter(grepl("pp111_cat", AOI)) %>%
  arrange(`Eye movement type index`) %>%
  select(`Eye movement type index`, AOI, Region, duration) %>%
  print(n = 30)

# STEP 7: NORMALISE + STATISTICAL MODELS
# Normalising the data before applying any statistics 
# Log transformation is applied to approximate normality and reduce influence outliers to selective path regression. 
# NA are automatically preserved as log (NA)=NA
# Logit link fucntion for first pass regression is applied later in the model to be normalised

# Log transform selective path regression
spr_r3_combined <- spr_r3_combined %>%
  mutate(log_spr_r3 = log(selective_path_regression))

spr_r4_combined <- spr_r4_combined %>%
  mutate(log_spr_r4 = log(selective_path_regression_r4))

# Checking normality of the log-transformed variables visually and statistically before running the LME
# Define function
check_normality <- function(data, var, title) {
  # Histogram: shows the shape of the distribution.
  # After log transformation we expect a roughly bell-shaped distribution.
  p1 <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(title = paste("Histogram -", title), x = var, y = "Count") +
    theme_minimal()
  
  # Q-Q plot: compares the data against a perfect normal distribution.
  # Points should follow the red diagonal line closely if data is normal.
  p2 <- ggplot(data, aes(sample = .data[[var]])) +
    stat_qq() +
    stat_qq_line(color = "firebrick") +
    labs(title = paste("Q-Q Plot -", title)) +
    theme_minimal()
  
  p1 | p2
}

# Check normality
check_normality(spr_r3_combined, "log_spr_r3", "SPR R3")
check_normality(spr_r4_combined, "log_spr_r4", "SPR R4")

# Shapiro-Wilk test (no quite sensitive for big samples)
shapiro.test(na.omit(spr_r3_combined$log_spr_r3))
shapiro.test(na.omit(spr_r4_combined$log_spr_r4))

names(spr_r3_combined)
names(spr_r4_combined)
names(fpr_r3_combined)
names(fpr_r4_combined)

# Calculate number of words per region per sentence from Excel (I need this to then calculate the number of words as covariate control)
n_words_lookup <- regions_raw %>%
  select(TOI, Region, Word_index) %>%
  distinct() %>%
  group_by(TOI, Region) %>%
  summarise(n_words = n_distinct(Word_index), .groups = "drop") %>%
  mutate(text_id = TOI) %>%
  select(text_id, Region, n_words)

# Add n_words for r3
n_words_r3 <- n_words_lookup %>%
  filter(Region == "r3") %>%
  rename(n_words_r3 = n_words) %>%
  select(text_id, n_words_r3)

# Add n_words for r4
n_words_r4 <- n_words_lookup %>%
  filter(Region == "r4") %>%
  rename(n_words_r4 = n_words) %>%
  select(text_id, n_words_r4)

# Join to each combined dataframe
spr_r3_combined <- spr_r3_combined %>%
  left_join(n_words_r3, by = "text_id")

spr_r4_combined <- spr_r4_combined %>%
  left_join(n_words_r4, by = "text_id")

fpr_r3_combined <- fpr_r3_combined %>%
  left_join(n_words_r3, by = "text_id")

fpr_r4_combined <- fpr_r4_combined %>%
  left_join(n_words_r4, by = "text_id")

# Verify
cat("NAs in n_words_r3 for SPR:", sum(is.na(spr_r3_combined$n_words_r3)), "\n")
cat("NAs in n_words_r4 for SPR:", sum(is.na(spr_r4_combined$n_words_r4)), "\n")
cat("NAs in n_words_r3 for FPR:", sum(is.na(fpr_r3_combined$n_words_r3)), "\n")
cat("NAs in n_words_r4 for FPR:", sum(is.na(fpr_r4_combined$n_words_r4)), "\n")

# Sum coding for Condition
# Ambiguous = -0.5, Control = 0.5
# Intercept represents grand mean, estimate reflects ambiguity effect directly
spr_r3_combined$Condition <- factor(spr_r3_combined$Condition)
contrasts(spr_r3_combined$Condition) <- c(-0.5, 0.5)

spr_r4_combined$Condition <- factor(spr_r4_combined$Condition)
contrasts(spr_r4_combined$Condition) <- c(-0.5, 0.5)

fpr_r3_combined$Condition <- factor(fpr_r3_combined$Condition)
contrasts(fpr_r3_combined$Condition) <- c(-0.5, 0.5)

fpr_r4_combined$Condition <- factor(fpr_r4_combined$Condition)
contrasts(fpr_r4_combined$Condition) <- c(-0.5, 0.5)

# SPR R3 - LME
lme_spr_r3_cat_pp <- lmer(log_spr_r3 ~ Condition + n_words_r3 +
                            (1|`Participant name`) + (1|text_id),
                          data = filter(spr_r3_combined, Language == "Catalan", Structure == "PP"),
                          REML = TRUE)
summary(lme_spr_r3_cat_pp)

lme_spr_r3_cat_rc <- lmer(log_spr_r3 ~ Condition + n_words_r3 +
                            (1|`Participant name`) + (1|text_id),
                          data = filter(spr_r3_combined, Language == "Catalan", Structure == "RC"),
                          REML = TRUE)
summary(lme_spr_r3_cat_rc)

lme_spr_r3_eng_pp <- lmer(log_spr_r3 ~ Condition + n_words_r3 +
                            (1|`Participant name`) + (1|text_id),
                          data = filter(spr_r3_combined, Language == "English", Structure == "PP"),
                          REML = TRUE)
summary(lme_spr_r3_eng_pp)

lme_spr_r3_eng_rc <- lmer(log_spr_r3 ~ Condition + n_words_r3 +
                            (1|`Participant name`) + (1|text_id),
                          data = filter(spr_r3_combined, Language == "English", Structure == "RC"),
                          REML = TRUE)
summary(lme_spr_r3_eng_rc)

# SPR R4 - LME
lme_spr_r4_cat_pp <- lmer(log_spr_r4 ~ Condition + n_words_r4 +
                            (1|`Participant name`) + (1|text_id),
                          data = filter(spr_r4_combined, Language == "Catalan", Structure == "PP"),
                          REML = TRUE)
summary(lme_spr_r4_cat_pp)

lme_spr_r4_cat_rc <- lmer(log_spr_r4 ~ Condition + n_words_r4 +
                            (1|`Participant name`) + (1|text_id),
                          data = filter(spr_r4_combined, Language == "Catalan", Structure == "RC"),
                          REML = TRUE)
summary(lme_spr_r4_cat_rc)

lme_spr_r4_eng_pp <- lmer(log_spr_r4 ~ Condition + n_words_r4 +
                            (1|`Participant name`) + (1|text_id),
                          data = filter(spr_r4_combined, Language == "English", Structure == "PP"),
                          REML = TRUE)
summary(lme_spr_r4_eng_pp)

lme_spr_r4_eng_rc <- lmer(log_spr_r4 ~ Condition + n_words_r4 +
                            (1|`Participant name`) + (1|text_id),
                          data = filter(spr_r4_combined, Language == "English", Structure == "RC"),
                          REML = TRUE)
summary(lme_spr_r4_eng_rc)

# FPR R3 - Logistic Mixed Models (logit link)
glme_fpr_r3_cat_pp <- glmer(firstpass_regression_r3 ~ Condition + n_words_r3 +
                              (1|`Participant name`) + (1|text_id),
                            data = filter(fpr_r3_combined, Language == "Catalan", Structure == "PP"),
                            family = binomial(link = "logit"))
summary(glme_fpr_r3_cat_pp)

glme_fpr_r3_cat_rc <- glmer(firstpass_regression_r3 ~ Condition + n_words_r3 +
                              (1|`Participant name`) + (1|text_id),
                            data = filter(fpr_r3_combined, Language == "Catalan", Structure == "RC"),
                            family = binomial(link = "logit"))
summary(glme_fpr_r3_cat_rc)

glme_fpr_r3_eng_pp <- glmer(firstpass_regression_r3 ~ Condition + n_words_r3 +
                              (1|`Participant name`) + (1|text_id),
                            data = filter(fpr_r3_combined, Language == "English", Structure == "PP"),
                            family = binomial(link = "logit"))
summary(glme_fpr_r3_eng_pp)

glme_fpr_r3_eng_rc <- glmer(firstpass_regression_r3 ~ Condition + n_words_r3 +
                              (1|`Participant name`) + (1|text_id),
                            data = filter(fpr_r3_combined, Language == "English", Structure == "RC"),
                            family = binomial(link = "logit"))
summary(glme_fpr_r3_eng_rc)

# ---- FPR R4 - Logistic Mixed Models (logit link) ----
glme_fpr_r4_cat_pp <- glmer(firstpass_regression_r4 ~ Condition + n_words_r4 +
                              (1|`Participant name`) + (1|text_id),
                            data = filter(fpr_r4_combined, Language == "Catalan", Structure == "PP"),
                            family = binomial(link = "logit"))
summary(glme_fpr_r4_cat_pp)

glme_fpr_r4_cat_rc <- glmer(firstpass_regression_r4 ~ Condition + n_words_r4 +
                              (1|`Participant name`) + (1|text_id),
                            data = filter(fpr_r4_combined, Language == "Catalan", Structure == "RC"),
                            family = binomial(link = "logit"))
summary(glme_fpr_r4_cat_rc)

glme_fpr_r4_eng_pp <- glmer(firstpass_regression_r4 ~ Condition + n_words_r4 +
                              (1|`Participant name`) + (1|text_id),
                            data = filter(fpr_r4_combined, Language == "English", Structure == "PP"),
                            family = binomial(link = "logit"))
summary(glme_fpr_r4_eng_pp)

glme_fpr_r4_eng_rc <- glmer(firstpass_regression_r4 ~ Condition + n_words_r4 +
                              (1|`Participant name`) + (1|text_id),
                            data = filter(fpr_r4_combined, Language == "English", Structure == "RC"),
                            family = binomial(link = "logit"))
summary(glme_fpr_r4_eng_rc)

#Extract results
extract_lme <- function(model, model_name) {
  coefs <- summary(model)$coefficients
  data.frame(
    Model = model_name,
    Effect = rownames(coefs),
    Beta = round(coefs[, "Estimate"], 3),
    SE = round(coefs[, "Std. Error"], 3),
    t = round(coefs[, "t value"], 3),
    p = round(coefs[, "Pr(>|t|)"], 3)
  )
}

extract_glme <- function(model, model_name) {
  coefs <- summary(model)$coefficients
  data.frame(
    Model = model_name,
    Effect = rownames(coefs),
    Beta = round(coefs[, "Estimate"], 3),
    SE = round(coefs[, "Std. Error"], 3),
    z = round(coefs[, "z value"], 3),
    p = round(coefs[, "Pr(>|z|)"], 3)
  )
}

# SPR R3 results
spr_r3_results <- rbind(
  extract_lme(lme_spr_r3_cat_pp, "Catalan PP"),
  extract_lme(lme_spr_r3_cat_rc, "Catalan RC"),
  extract_lme(lme_spr_r3_eng_pp, "English PP"),
  extract_lme(lme_spr_r3_eng_rc, "English RC")
)
View(spr_r3_results)

# SPR R4 results
spr_r4_results <- rbind(
  extract_lme(lme_spr_r4_cat_pp, "Catalan PP"),
  extract_lme(lme_spr_r4_cat_rc, "Catalan RC"),
  extract_lme(lme_spr_r4_eng_pp, "English PP"),
  extract_lme(lme_spr_r4_eng_rc, "English RC")
)
View(spr_r4_results)

# FPR R3 results
fpr_r3_results <- rbind(
  extract_glme(glme_fpr_r3_cat_pp, "Catalan PP"),
  extract_glme(glme_fpr_r3_cat_rc, "Catalan RC"),
  extract_glme(glme_fpr_r3_eng_pp, "English PP"),
  extract_glme(glme_fpr_r3_eng_rc, "English RC")
)
View(fpr_r3_results)

# FPR R4 results
fpr_r4_results <- rbind(
  extract_glme(glme_fpr_r4_cat_pp, "Catalan PP"),
  extract_glme(glme_fpr_r4_cat_rc, "Catalan RC"),
  extract_glme(glme_fpr_r4_eng_pp, "English PP"),
  extract_glme(glme_fpr_r4_eng_rc, "English RC")
)
View(fpr_r4_results)

