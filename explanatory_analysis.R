
install.packages("arrow") #only run it the first time
install.packages ("dplyr")
install.packages ("tidyr")
install.packages ("readxl")
install.packages ("stringr")
install.packages ("duckdb")
install.packages ("rstudioapi")
install.packages("ggplot2")
library(arrow)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(duckdb)
library (rstudioapi)
library (ggplot2)
library(extrafont)
library(patchwork)


rc <- read_parquet("rc_long_filtered.parquet")
# Merging the Excel file, where the regions are with the raw data so that each word has its corresponding region. 
setwd (dirname(rstudioapi::getActiveDocumentContext()$path))

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

# Merge region function
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

cat("RC - NAs in Region:", sum(is.na(rc_regions$Region)), "\n")

# Extract sentence info + RC_type classificastion

rc_regions <- rc_regions %>%
  mutate(
    text_id = sub(" - .*", "", AOI),
    Condition = ifelse(str_detect(text_id, "ctr"), "Control", "Ambiguous"),
    Language = case_when(
      str_detect(text_id, "cat") ~ "Catalan",
      str_detect(text_id, "eng") ~ "English",
      TRUE ~ "Other"
    ),
    Type = "RC",
    RC_type = case_when(
      text_id %in% c("rc24_eng", "rc25_eng", "rc26_eng", "rc27_eng",
                     "rc28_eng", "rc29_eng", "rc30_eng",
                     "ctrrc24_eng", "ctrrc25_eng", "ctrrc26_eng", "ctrrc27_eng",
                     "ctrrc28_eng", "ctrrc29_eng", "ctrrc30_eng") ~ "Object_RC",
      text_id %in% c("rc133_cat", "rc134_cat", "rc135_cat", "rc136_cat",
                     "rc137_cat", "rc138_cat", "rc139_cat", "rc140_cat",
                     "ctrrc133_cat", "ctrrc134_cat", "ctrrc135_cat", "ctrrc136_cat",
                     "ctrrc137_cat", "ctrrc138_cat", "ctrrc139_cat", "ctrrc140_cat") ~ "Object_RC",
      TRUE ~ "Subject_RC"
    )
  )

View(rc_regions)
# Calculating metrics: SPR (Selective path regression), FPR (first-pass regression)
# SPR R3 for RC with RC_type
spr_rc_r3 <- rc_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type, RC_type) %>%
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

# SPR R4 for RC with RC_type
spr_rc_r4 <- rc_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type, RC_type) %>%
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

# FPR R3 for RC with RC_type
fpr_rc_r3 <- rc_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type, RC_type) %>%
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

# FPR R4 for RC with RC_type
fpr_rc_r4 <- rc_regions %>%
  group_by(`Participant name`, text_id, Condition, Language, Type, RC_type) %>%
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

# Descriptive graphs 
# calc_summary to include RC_type
calc_summary_rc <- function(data, measure_col) {
  data %>%
    filter(!is.na(.data[[measure_col]]), Structure == "RC") %>%
    group_by(Condition, Language, Structure, RC_type) %>%
    summarise(
      mean_val = mean(.data[[measure_col]], na.rm = TRUE),
      se_val = sd(.data[[measure_col]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
}

# Combine rc for each measure
spr_r3_combined <- bind_rows(
  spr_rc_r3 %>% mutate(Structure = "RC")
)

spr_r4_combined <- bind_rows(
  spr_rc_r4 %>% mutate(Structure = "RC")
)

fpr_r3_combined <- bind_rows(
  fpr_rc_r3 %>% mutate(Structure = "RC")
)

fpr_r4_combined <- bind_rows(
  fpr_rc_r4 %>% mutate(Structure = "RC")
)

#  make_plot for RC_type (x axis = RC_type)
make_plot_rc <- function(data, y_var, se_var, title, y_label, y_limits) {
  ggplot(data, aes(x = RC_type, y = .data[[y_var]], fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
    geom_errorbar(aes(ymin = .data[[y_var]] - .data[[se_var]],
                      ymax = .data[[y_var]] + .data[[se_var]]),
                  position = position_dodge(0.8), width = 0.25) +
    scale_fill_manual(values = condition_colors) +
    scale_y_continuous(limits = y_limits) +
    labs(title = title, x = "RC Type", y = y_label) +
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
condition_colors <- c("Control" = "steelblue", "Ambiguous" = "firebrick")
# SPR R3 - Object vs Subject RC
spr_r3_rc_summary <- calc_summary_rc(spr_r3_combined, "selective_path_regression")
y_max_spr_r3_rc <- max(spr_r3_rc_summary$mean_val + spr_r3_rc_summary$se_val) * 1.1

p_spr_r3_rc_cat <- make_plot_rc(
  filter(spr_r3_rc_summary, Language == "Catalan"),
  "mean_val", "se_val", "Catalan",
  "Mean Selective Path Regression (ms)", c(0, y_max_spr_r3_rc)
)
p_spr_r3_rc_eng <- make_plot_rc(
  filter(spr_r3_rc_summary, Language == "English"),
  "mean_val", "se_val", "English",
  "Mean Selective Path Regression (ms)", c(0, y_max_spr_r3_rc)
)

figure_spr_r3_rc <- (p_spr_r3_rc_cat | p_spr_r3_rc_eng) +
  plot_annotation(
    caption = "Figure X. Mean selective path regression (ms) in R3 for Subject and Object RC\nin Catalan (left) and English (right). Error bars represent standard error.\nBlue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("Figure_SPR_R3_RC_type.png", figure_spr_r3_rc, width = 18, height = 10, units = "cm", dpi = 300)

# SPR R4 - Object vs Subject RC
spr_r4_rc_summary <- calc_summary_rc(spr_r4_combined, "selective_path_regression_r4")
y_max_spr_r4_rc <- max(spr_r4_rc_summary$mean_val + spr_r4_rc_summary$se_val) * 1.1

p_spr_r4_rc_cat <- make_plot_rc(
  filter(spr_r4_rc_summary, Language == "Catalan"),
  "mean_val", "se_val", "Catalan",
  "Mean Selective Path Regression (ms)", c(0, y_max_spr_r4_rc)
)
p_spr_r4_rc_eng <- make_plot_rc(
  filter(spr_r4_rc_summary, Language == "English"),
  "mean_val", "se_val", "English",
  "Mean Selective Path Regression (ms)", c(0, y_max_spr_r4_rc)
)

figure_spr_r4_rc <- (p_spr_r4_rc_cat | p_spr_r4_rc_eng) +
  plot_annotation(
    caption = "Figure X. Mean selective path regression (ms) in R4 for Subject and Object RC\nin Catalan (left) and English (right). Error bars represent standard error.\nBlue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("Figure_SPR_R4_RC_type.png", figure_spr_r4_rc, width = 18, height = 10, units = "cm", dpi = 300)

# FPR R3 - Object vs Subject RC
fpr_r3_combined <- fpr_r3_combined %>%
  mutate(firstpass_pct = firstpass_regression_r3 * 100)

fpr_r3_rc_summary <- calc_summary_rc(fpr_r3_combined, "firstpass_pct")
y_max_fpr_r3_rc <- max(fpr_r3_rc_summary$mean_val + fpr_r3_rc_summary$se_val) * 1.1

p_fpr_r3_rc_cat <- make_plot_rc(
  filter(fpr_r3_rc_summary, Language == "Catalan"),
  "mean_val", "se_val", "Catalan",
  "First-pass Regression (%)", c(0, y_max_fpr_r3_rc)
)
p_fpr_r3_rc_eng <- make_plot_rc(
  filter(fpr_r3_rc_summary, Language == "English"),
  "mean_val", "se_val", "English",
  "First-pass Regression (%)", c(0, y_max_fpr_r3_rc)
)

figure_fpr_r3_rc <- (p_fpr_r3_rc_cat | p_fpr_r3_rc_eng) +
  plot_annotation(
    caption = "Figure X. First-pass regression (%) in R3 for Subject and Object RC\nin Catalan (left) and English (right). Error bars represent standard error.\nBlue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("Figure_FPR_R3_RC_type.png", figure_fpr_r3_rc, width = 18, height = 10, units = "cm", dpi = 300)

# FPR R4 - Object vs Subject RC
fpr_r4_combined <- fpr_r4_combined %>%
  mutate(firstpass_pct = firstpass_regression_r4 * 100)

fpr_r4_rc_summary <- calc_summary_rc(fpr_r4_combined, "firstpass_pct")
y_max_fpr_r4_rc <- max(fpr_r4_rc_summary$mean_val + fpr_r4_rc_summary$se_val) * 1.1

p_fpr_r4_rc_cat <- make_plot_rc(
  filter(fpr_r4_rc_summary, Language == "Catalan"),
  "mean_val", "se_val", "Catalan",
  "First-pass Regression (%)", c(0, y_max_fpr_r4_rc)
)
p_fpr_r4_rc_eng <- make_plot_rc(
  filter(fpr_r4_rc_summary, Language == "English"),
  "mean_val", "se_val", "English",
  "First-pass Regression (%)", c(0, y_max_fpr_r4_rc)
)

figure_fpr_r4_rc <- (p_fpr_r4_rc_cat | p_fpr_r4_rc_eng) +
  plot_annotation(
    caption = "Figure X. First-pass regression (%) in R4 for Subject and Object RC\nin Catalan (left) and English (right). Error bars represent standard error.\nBlue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("Figure_FPR_R4_RC_type.png", figure_fpr_r4_rc, width = 18, height = 10, units = "cm", dpi = 300)

cat("All RC type figures saved!\n")

# Normalising the data before applying any statistics 

# Log transform SPR
spr_r3_combined <- spr_r3_combined %>%
  mutate(log_spr_r3 = log(selective_path_regression))

spr_r4_combined <- spr_r4_combined %>%
  mutate(log_spr_r4 = log(selective_path_regression_r4))

# n_words_r3 and n_words_r4 are calculated from the regions Excel file (already loaded as 'regions')
# and added as covariates to control for sentence length differences across regions.
# This is needed for the LME models but not for the descriptive plots.
n_words_lookup <- regions %>%
  select(TOI, AOI, Region) %>%
  distinct() %>%
  group_by(TOI, Region) %>%
  summarise(n_words = n_distinct(AOI), .groups = "drop") %>%
  mutate(text_id = TOI) %>%
  select(text_id, Region, n_words)

n_words_r3 <- n_words_lookup %>%
  filter(Region == "r3") %>%
  rename(n_words_r3 = n_words) %>%
  select(text_id, n_words_r3)

n_words_r4 <- n_words_lookup %>%
  filter(Region == "r4") %>%
  rename(n_words_r4 = n_words) %>%
  select(text_id, n_words_r4)

spr_r3_combined <- spr_r3_combined %>%
  left_join(n_words_r3, by = "text_id")

spr_r4_combined <- spr_r4_combined %>%
  left_join(n_words_r4, by = "text_id")

fpr_r3_combined <- fpr_r3_combined %>%
  left_join(n_words_r3, by = "text_id")

fpr_r4_combined <- fpr_r4_combined %>%
  left_join(n_words_r4, by = "text_id")

# Sum coding for Condition
# Ambiguous = -0.5, Control = 0
spr_r3_combined$Condition <- factor(spr_r3_combined$Condition)
contrasts(spr_r3_combined$Condition) <- c(-0.5, 0.5)

spr_r4_combined$Condition <- factor(spr_r4_combined$Condition)
contrasts(spr_r4_combined$Condition) <- c(-0.5, 0.5)

fpr_r3_combined$Condition <- factor(fpr_r3_combined$Condition)
contrasts(fpr_r3_combined$Condition) <- c(-0.5, 0.5)

fpr_r4_combined$Condition <- factor(fpr_r4_combined$Condition)
contrasts(fpr_r4_combined$Condition) <- c(-0.5, 0.5)

# Upload libraries in order to the LME
#install.packages("lme4")
#install.packages("lmerTest")
library(lme4)
library(lmerTest)

# Run LME tests
# SPR R3 - LME by RC type
lme_spr_r3_cat_subj_rc <- lmer(log_spr_r3 ~ Condition + n_words_r3 +
                                 (1|`Participant name`) + (1|text_id),
                               data = filter(spr_r3_combined, Language == "Catalan", Structure == "RC", RC_type == "Subject_RC"),
                               REML = TRUE)
summary(lme_spr_r3_cat_subj_rc)

lme_spr_r3_cat_obj_rc <- lmer(log_spr_r3 ~ Condition + n_words_r3 +
                                (1|`Participant name`) + (1|text_id),
                              data = filter(spr_r3_combined, Language == "Catalan", Structure == "RC", RC_type == "Object_RC"),
                              REML = TRUE)
summary(lme_spr_r3_cat_obj_rc)

lme_spr_r3_eng_subj_rc <- lmer(log_spr_r3 ~ Condition + n_words_r3 +
                                 (1|`Participant name`) + (1|text_id),
                               data = filter(spr_r3_combined, Language == "English", Structure == "RC", RC_type == "Subject_RC"),
                               REML = TRUE)
summary(lme_spr_r3_eng_subj_rc)

lme_spr_r3_eng_obj_rc <- lmer(log_spr_r3 ~ Condition + n_words_r3 +
                                (1|`Participant name`) + (1|text_id),
                              data = filter(spr_r3_combined, Language == "English", Structure == "RC", RC_type == "Object_RC"),
                              REML = TRUE)
summary(lme_spr_r3_eng_obj_rc)

# SPR R4 - LME by RC type
lme_spr_r4_cat_subj_rc <- lmer(log_spr_r4 ~ Condition + n_words_r4 +
                                 (1|`Participant name`) + (1|text_id),
                               data = filter(spr_r4_combined, Language == "Catalan", Structure == "RC", RC_type == "Subject_RC"),
                               REML = TRUE)
summary(lme_spr_r4_cat_subj_rc)

lme_spr_r4_cat_obj_rc <- lmer(log_spr_r4 ~ Condition + n_words_r4 +
                                (1|`Participant name`) + (1|text_id),
                              data = filter(spr_r4_combined, Language == "Catalan", Structure == "RC", RC_type == "Object_RC"),
                              REML = TRUE)
summary(lme_spr_r4_cat_obj_rc)

lme_spr_r4_eng_subj_rc <- lmer(log_spr_r4 ~ Condition + n_words_r4 +
                                 (1|`Participant name`) + (1|text_id),
                               data = filter(spr_r4_combined, Language == "English", Structure == "RC", RC_type == "Subject_RC"),
                               REML = TRUE)
summary(lme_spr_r4_eng_subj_rc)

lme_spr_r4_eng_obj_rc <- lmer(log_spr_r4 ~ Condition + n_words_r4 +
                                (1|`Participant name`) + (1|text_id),
                              data = filter(spr_r4_combined, Language == "English", Structure == "RC", RC_type == "Object_RC"),
                              REML = TRUE)
summary(lme_spr_r4_eng_obj_rc)

# FPR R3 - Logistic by RC type
glme_fpr_r3_cat_subj_rc <- glmer(firstpass_regression_r3 ~ Condition + n_words_r3 +
                                   (1|`Participant name`) + (1|text_id),
                                 data = filter(fpr_r3_combined, Language == "Catalan", Structure == "RC", RC_type == "Subject_RC"),
                                 family = binomial(link = "logit"))
summary(glme_fpr_r3_cat_subj_rc)

glme_fpr_r3_cat_obj_rc <- glmer(firstpass_regression_r3 ~ Condition + n_words_r3 +
                                  (1|`Participant name`) + (1|text_id),
                                data = filter(fpr_r3_combined, Language == "Catalan", Structure == "RC", RC_type == "Object_RC"),
                                family = binomial(link = "logit"))
summary(glme_fpr_r3_cat_obj_rc)

glme_fpr_r3_eng_subj_rc <- glmer(firstpass_regression_r3 ~ Condition + n_words_r3 +
                                   (1|`Participant name`) + (1|text_id),
                                 data = filter(fpr_r3_combined, Language == "English", Structure == "RC", RC_type == "Subject_RC"),
                                 family = binomial(link = "logit"))
summary(glme_fpr_r3_eng_subj_rc)

glme_fpr_r3_eng_obj_rc <- glmer(firstpass_regression_r3 ~ Condition + n_words_r3 +
                                  (1|`Participant name`) + (1|text_id),
                                data = filter(fpr_r3_combined, Language == "English", Structure == "RC", RC_type == "Object_RC"),
                                family = binomial(link = "logit"))
summary(glme_fpr_r3_eng_obj_rc)

# FPR R4 - Logistic by RC type
glme_fpr_r4_cat_subj_rc <- glmer(firstpass_regression_r4 ~ Condition + n_words_r4 +
                                   (1|`Participant name`) + (1|text_id),
                                 data = filter(fpr_r4_combined, Language == "Catalan", Structure == "RC", RC_type == "Subject_RC"),
                                 family = binomial(link = "logit"))
summary(glme_fpr_r4_cat_subj_rc)

glme_fpr_r4_cat_obj_rc <- glmer(firstpass_regression_r4 ~ Condition + n_words_r4 +
                                  (1|`Participant name`) + (1|text_id),
                                data = filter(fpr_r4_combined, Language == "Catalan", Structure == "RC", RC_type == "Object_RC"),
                                family = binomial(link = "logit"))
summary(glme_fpr_r4_cat_obj_rc)

glme_fpr_r4_eng_subj_rc <- glmer(firstpass_regression_r4 ~ Condition + n_words_r4 +
                                   (1|`Participant name`) + (1|text_id),
                                 data = filter(fpr_r4_combined, Language == "English", Structure == "RC", RC_type == "Subject_RC"),
                                 family = binomial(link = "logit"))
summary(glme_fpr_r4_eng_subj_rc)

glme_fpr_r4_eng_obj_rc <- glmer(firstpass_regression_r4 ~ Condition + n_words_r4 +
                                  (1|`Participant name`) + (1|text_id),
                                data = filter(fpr_r4_combined, Language == "English", Structure == "RC", RC_type == "Object_RC"),
                                family = binomial(link = "logit"))
summary(glme_fpr_r4_eng_obj_rc)

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

# SPR results
spr_r3_rc_results <- rbind(
  extract_lme(lme_spr_r3_cat_subj_rc, "Catalan Subject RC"),
  extract_lme(lme_spr_r3_cat_obj_rc, "Catalan Object RC"),
  extract_lme(lme_spr_r3_eng_subj_rc, "English Subject RC"),
  extract_lme(lme_spr_r3_eng_obj_rc, "English Object RC")
)
View(spr_r3_rc_results)

spr_r4_rc_results <- rbind(
  extract_lme(lme_spr_r4_cat_subj_rc, "Catalan Subject RC"),
  extract_lme(lme_spr_r4_cat_obj_rc, "Catalan Object RC"),
  extract_lme(lme_spr_r4_eng_subj_rc, "English Subject RC"),
  extract_lme(lme_spr_r4_eng_obj_rc, "English Object RC")
)
View(spr_r4_rc_results)

# FPR results 
fpr_r3_rc_results <- rbind(
  extract_glme(glme_fpr_r3_cat_subj_rc, "Catalan Subject RC"),
  extract_glme(glme_fpr_r3_cat_obj_rc, "Catalan Object RC"),
  extract_glme(glme_fpr_r3_eng_subj_rc, "English Subject RC"),
  extract_glme(glme_fpr_r3_eng_obj_rc, "English Object RC")
)
View(fpr_r3_rc_results)

fpr_r4_rc_results <- rbind(
  extract_glme(glme_fpr_r4_cat_subj_rc, "Catalan Subject RC"),
  extract_glme(glme_fpr_r4_cat_obj_rc, "Catalan Object RC"),
  extract_glme(glme_fpr_r4_eng_subj_rc, "English Subject RC"),
  extract_glme(glme_fpr_r4_eng_obj_rc, "English Object RC")
)
View(fpr_r4_rc_results)

# First-pass duration and total duration fixations.
# Load tobii_metrics.xlsx to calculate first-pass and total fixation duration by RC_type
df <- read_excel("tobii_metrics.xlsx")

# Categorize sentences including RC_type
df_final_columns <- df %>%
  mutate(
    Condition = ifelse(str_detect(TOI, "ctr"), "Control", "Ambiguous"),
    Language = case_when(
      str_detect(TOI, "cat") ~ "Catalan",
      str_detect(TOI, "eng") ~ "English",
      TRUE ~ "Other"
    ),
    Type = case_when(
      str_detect(TOI, "pp") ~ "PP",
      str_detect(TOI, "rc") ~ "RC",
      TRUE ~ "Filler"
    ),
    RC_type = case_when(
      TOI %in% c("rc24_eng", "rc25_eng", "rc26_eng", "rc27_eng",
                 "rc28_eng", "rc29_eng", "rc30_eng",
                 "ctrrc24_eng", "ctrrc25_eng", "ctrrc26_eng", "ctrrc27_eng",
                 "ctrrc28_eng", "ctrrc29_eng", "ctrrc30_eng",
                 "rc133_cat", "rc134_cat", "rc135_cat", "rc136_cat",
                 "rc137_cat", "rc138_cat", "rc139_cat", "rc140_cat",
                 "ctrrc133_cat", "ctrrc134_cat", "ctrrc135_cat", "ctrrc136_cat",
                 "ctrrc137_cat", "ctrrc138_cat", "ctrrc139_cat", "ctrrc140_cat") ~ "Object_RC",
      str_detect(TOI, "rc") ~ "Subject_RC",
      TRUE ~ NA_character_
    )
  )

# R3 first-pass and total fixation duration by RC_type
df_r3_rc <- df_final_columns %>%
  filter(Type == "RC") %>%
  group_by(Participant, Language, Condition, RC_type, TOI) %>%
  summarize(
    R3_Total_Time = sum(Total_duration_of_fixations[Region == "r3"], na.rm = TRUE),
    R3_First_Pass = sum(Firstpass_duration[Region == "r3"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    R3_First_Pass = replace(R3_First_Pass, R3_First_Pass == 0, NA),
    R3_Total_Time = replace(R3_Total_Time, R3_Total_Time == 0, NA)
  )

# R4 first-pass and total fixation duration by RC_type
df_r4_rc <- df_final_columns %>%
  filter(Type == "RC") %>%
  group_by(Participant, Language, Condition, RC_type, TOI) %>%
  summarize(
    R4_Total_Time = sum(Total_duration_of_fixations[Region == "r4"], na.rm = TRUE),
    R4_First_Pass = sum(Firstpass_duration[Region == "r4"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    R4_First_Pass = replace(R4_First_Pass, R4_First_Pass == 0, NA),
    R4_Total_Time = replace(R4_Total_Time, R4_Total_Time == 0, NA)
  )

# Descriptive summaries
r3_fp_rc_summary <- df_r3_rc %>%
  filter(!is.na(R3_First_Pass)) %>%
  group_by(Language, Condition, RC_type) %>%
  summarise(
    mean_fp = mean(R3_First_Pass, na.rm = TRUE),
    se_fp   = sd(R3_First_Pass, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

r3_total_rc_summary <- df_r3_rc %>%
  filter(!is.na(R3_Total_Time)) %>%
  group_by(Language, Condition, RC_type) %>%
  summarise(
    mean_total = mean(R3_Total_Time, na.rm = TRUE),
    se_total   = sd(R3_Total_Time, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

r4_fp_rc_summary <- df_r4_rc %>%
  filter(!is.na(R4_First_Pass)) %>%
  group_by(Language, Condition, RC_type) %>%
  summarise(
    mean_fp = mean(R4_First_Pass, na.rm = TRUE),
    se_fp   = sd(R4_First_Pass, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

r4_total_rc_summary <- df_r4_rc %>%
  filter(!is.na(R4_Total_Time)) %>%
  group_by(Language, Condition, RC_type) %>%
  summarise(
    mean_total = mean(R4_Total_Time, na.rm = TRUE),
    se_total   = sd(R4_Total_Time, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

View(r3_fp_rc_summary)
View(r3_total_rc_summary)
View(r4_fp_rc_summary)
View(r4_total_rc_summary)

# Descriptive stats
# make_plot for RC_type (x axis = RC_type)
make_plot_rc_fp <- function(data, y_var, se_var, title, y_label, y_limits) {
  ggplot(data, aes(x = RC_type, y = .data[[y_var]], fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
    geom_errorbar(aes(ymin = .data[[y_var]] - .data[[se_var]],
                      ymax = .data[[y_var]] + .data[[se_var]]),
                  position = position_dodge(0.8), width = 0.25) +
    scale_fill_manual(values = condition_colors) +
    scale_y_continuous(limits = y_limits) +
    labs(title = title, x = "RC Type", y = y_label) +
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

# Figure: R3 First-pass duration by RC_type
y_max_r3_fp_rc <- max(r3_fp_rc_summary$mean_fp + r3_fp_rc_summary$se_fp) * 1.1

p_r3_fp_rc_cat <- make_plot_rc_fp(
  filter(r3_fp_rc_summary, Language == "Catalan"),
  "mean_fp", "se_fp", "Catalan",
  "Mean First-pass Duration (ms)", c(0, y_max_r3_fp_rc)
)
p_r3_fp_rc_eng <- make_plot_rc_fp(
  filter(r3_fp_rc_summary, Language == "English"),
  "mean_fp", "se_fp", "English",
  "Mean First-pass Duration (ms)", c(0, y_max_r3_fp_rc)
)

figure_r3_fp_rc <- (p_r3_fp_rc_cat | p_r3_fp_rc_eng) +
  plot_annotation(
    caption = "Figure X. Mean first-pass duration (ms) in R3 for Subject and Object RC
in Catalan (left) and English (right). Error bars represent standard error.
Blue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

figure_r3_fp_rc
ggsave("Figure_R3_Firstpass_RCtype.png", figure_r3_fp_rc, width = 18, height = 10, units = "cm", dpi = 300)

# Figure: R3 Total fixation duration by RC_type
y_max_r3_total_rc <- max(r3_total_rc_summary$mean_total + r3_total_rc_summary$se_total) * 1.1

p_r3_total_rc_cat <- make_plot_rc_fp(
  filter(r3_total_rc_summary, Language == "Catalan"),
  "mean_total", "se_total", "Catalan",
  "Mean Total Fixation Duration (ms)", c(0, y_max_r3_total_rc)
)
p_r3_total_rc_eng <- make_plot_rc_fp(
  filter(r3_total_rc_summary, Language == "English"),
  "mean_total", "se_total", "English",
  "Mean Total Fixation Duration (ms)", c(0, y_max_r3_total_rc)
)

figure_r3_total_rc <- (p_r3_total_rc_cat | p_r3_total_rc_eng) +
  plot_annotation(
    caption = "Figure X. Mean total fixation duration (ms) in R3 for Subject and Object RC
in Catalan (left) and English (right). Error bars represent standard error.
Blue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

figure_r3_total_rc
ggsave("Figure_R3_Total_RCtype.png", figure_r3_total_rc, width = 18, height = 10, units = "cm", dpi = 300)

# Figure: R4 First-pass duration by RC_type
y_max_r4_fp_rc <- max(r4_fp_rc_summary$mean_fp + r4_fp_rc_summary$se_fp) * 1.1

p_r4_fp_rc_cat <- make_plot_rc_fp(
  filter(r4_fp_rc_summary, Language == "Catalan"),
  "mean_fp", "se_fp", "Catalan",
  "Mean First-pass Duration (ms)", c(0, y_max_r4_fp_rc)
)
p_r4_fp_rc_eng <- make_plot_rc_fp(
  filter(r4_fp_rc_summary, Language == "English"),
  "mean_fp", "se_fp", "English",
  "Mean First-pass Duration (ms)", c(0, y_max_r4_fp_rc)
)

figure_r4_fp_rc <- (p_r4_fp_rc_cat | p_r4_fp_rc_eng) +
  plot_annotation(
    caption = "Figure X. Mean first-pass duration (ms) in R4 for Subject and Object RC
in Catalan (left) and English (right). Error bars represent standard error.
Blue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

figure_r4_fp_rc
ggsave("Figure_R4_Firstpass_RCtype.png", figure_r4_fp_rc, width = 18, height = 10, units = "cm", dpi = 300)

# Figure: R4 Total fixation duration by RC_type
y_max_r4_total_rc <- max(r4_total_rc_summary$mean_total + r4_total_rc_summary$se_total) * 1.1

p_r4_total_rc_cat <- make_plot_rc_fp(
  filter(r4_total_rc_summary, Language == "Catalan"),
  "mean_total", "se_total", "Catalan",
  "Mean Total Fixation Duration (ms)", c(0, y_max_r4_total_rc)
)
p_r4_total_rc_eng <- make_plot_rc_fp(
  filter(r4_total_rc_summary, Language == "English"),
  "mean_total", "se_total", "English",
  "Mean Total Fixation Duration (ms)", c(0, y_max_r4_total_rc)
)

figure_r4_total_rc <- (p_r4_total_rc_cat | p_r4_total_rc_eng) +
  plot_annotation(
    caption = "Figure X. Mean total fixation duration (ms) in R4 for Subject and Object RC
in Catalan (left) and English (right). Error bars represent standard error.
Blue = Control, Red = Ambiguous.",
    theme = theme(plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

figure_r4_total_rc
ggsave("Figure_R4_Total_RCtype.png", figure_r4_total_rc, width = 18, height = 10, units = "cm", dpi = 300)

cat("All first-pass and total fixation RC type figures saved!\n")

# Narmalise data
# Log transformation for RC_type data
df_r3_rc <- df_r3_rc %>%
  mutate(
    log_R3_First_Pass = log(R3_First_Pass),
    log_R3_Total_Time = log(R3_Total_Time)
  )

df_r4_rc <- df_r4_rc %>%
  mutate(
    log_R4_First_Pass = log(R4_First_Pass),
    log_R4_Total_Time = log(R4_Total_Time)
  )

# Sum coding for Condition
df_r3_rc$Condition <- factor(df_r3_rc$Condition)
contrasts(df_r3_rc$Condition) <- c(-0.5, 0.5)  # Ambiguous = -0.5, Control = 0.5

df_r4_rc$Condition <- factor(df_r4_rc$Condition)
contrasts(df_r4_rc$Condition) <- c(-0.5, 0.5)

# Add word counts as covariate
r3_length_rc <- df_final_columns %>%
  filter(Region == "r3", Type == "RC") %>%
  group_by(TOI) %>%
  summarise(r3_n_words = max(Word_index) - min(Word_index) + 1, .groups = "drop")

r4_length_rc <- df_final_columns %>%
  filter(Region == "r4", Type == "RC") %>%
  group_by(TOI) %>%
  summarise(r4_n_words = max(Word_index) - min(Word_index) + 1, .groups = "drop")

df_r3_rc <- df_r3_rc %>% left_join(r3_length_rc, by = "TOI")
df_r4_rc <- df_r4_rc %>% left_join(r4_length_rc, by = "TOI")

# LME models - R3 First-pass duration by RC_type
lme_r3_fp_cat_subj_rc <- lmer(log_R3_First_Pass ~ Condition + r3_n_words +
                                (1|Participant) + (1|TOI),
                              data = filter(df_r3_rc, Language == "Catalan", RC_type == "Subject_RC"),
                              REML = TRUE)
summary(lme_r3_fp_cat_subj_rc)

lme_r3_fp_cat_obj_rc <- lmer(log_R3_First_Pass ~ Condition + r3_n_words +
                               (1|Participant) + (1|TOI),
                             data = filter(df_r3_rc, Language == "Catalan", RC_type == "Object_RC"),
                             REML = TRUE)
summary(lme_r3_fp_cat_obj_rc)

lme_r3_fp_eng_subj_rc <- lmer(log_R3_First_Pass ~ Condition + r3_n_words +
                                (1|Participant) + (1|TOI),
                              data = filter(df_r3_rc, Language == "English", RC_type == "Subject_RC"),
                              REML = TRUE)
summary(lme_r3_fp_eng_subj_rc)

lme_r3_fp_eng_obj_rc <- lmer(log_R3_First_Pass ~ Condition + r3_n_words +
                               (1|Participant) + (1|TOI),
                             data = filter(df_r3_rc, Language == "English", RC_type == "Object_RC"),
                             REML = TRUE)
summary(lme_r3_fp_eng_obj_rc)

# LME models - R3 Total fixation duration by RC_type
lme_r3_total_cat_subj_rc <- lmer(log_R3_Total_Time ~ Condition + r3_n_words +
                                   (1|Participant) + (1|TOI),
                                 data = filter(df_r3_rc, Language == "Catalan", RC_type == "Subject_RC"),
                                 REML = TRUE)
summary(lme_r3_total_cat_subj_rc)

lme_r3_total_cat_obj_rc <- lmer(log_R3_Total_Time ~ Condition + r3_n_words +
                                  (1|Participant) + (1|TOI),
                                data = filter(df_r3_rc, Language == "Catalan", RC_type == "Object_RC"),
                                REML = TRUE)
summary(lme_r3_total_cat_obj_rc)

lme_r3_total_eng_subj_rc <- lmer(log_R3_Total_Time ~ Condition + r3_n_words +
                                   (1|Participant) + (1|TOI),
                                 data = filter(df_r3_rc, Language == "English", RC_type == "Subject_RC"),
                                 REML = TRUE)
summary(lme_r3_total_eng_subj_rc)

lme_r3_total_eng_obj_rc <- lmer(log_R3_Total_Time ~ Condition + r3_n_words +
                                  (1|Participant) + (1|TOI),
                                data = filter(df_r3_rc, Language == "English", RC_type == "Object_RC"),
                                REML = TRUE)
summary(lme_r3_total_eng_obj_rc)

# LME models - R4 First-pass duration by RC_type
lme_r4_fp_cat_subj_rc <- lmer(log_R4_First_Pass ~ Condition + r4_n_words +
                                (1|Participant) + (1|TOI),
                              data = filter(df_r4_rc, Language == "Catalan", RC_type == "Subject_RC"),
                              REML = TRUE)
summary(lme_r4_fp_cat_subj_rc)

lme_r4_fp_cat_obj_rc <- lmer(log_R4_First_Pass ~ Condition + r4_n_words +
                               (1|Participant) + (1|TOI),
                             data = filter(df_r4_rc, Language == "Catalan", RC_type == "Object_RC"),
                             REML = TRUE)
summary(lme_r4_fp_cat_obj_rc)

lme_r4_fp_eng_subj_rc <- lmer(log_R4_First_Pass ~ Condition + r4_n_words +
                                (1|Participant) + (1|TOI),
                              data = filter(df_r4_rc, Language == "English", RC_type == "Subject_RC"),
                              REML = TRUE)
summary(lme_r4_fp_eng_subj_rc)

lme_r4_fp_eng_obj_rc <- lmer(log_R4_First_Pass ~ Condition + r4_n_words +
                               (1|Participant) + (1|TOI),
                             data = filter(df_r4_rc, Language == "English", RC_type == "Object_RC"),
                             REML = TRUE)
summary(lme_r4_fp_eng_obj_rc)

# LME models - R4 Total fixation duration by RC_type
lme_r4_total_cat_subj_rc <- lmer(log_R4_Total_Time ~ Condition + r4_n_words +
                                   (1|Participant) + (1|TOI),
                                 data = filter(df_r4_rc, Language == "Catalan", RC_type == "Subject_RC"),
                                 REML = TRUE)
summary(lme_r4_total_cat_subj_rc)

lme_r4_total_cat_obj_rc <- lmer(log_R4_Total_Time ~ Condition + r4_n_words +
                                  (1|Participant) + (1|TOI),
                                data = filter(df_r4_rc, Language == "Catalan", RC_type == "Object_RC"),
                                REML = TRUE)
summary(lme_r4_total_cat_obj_rc)

lme_r4_total_eng_subj_rc <- lmer(log_R4_Total_Time ~ Condition + r4_n_words +
                                   (1|Participant) + (1|TOI),
                                 data = filter(df_r4_rc, Language == "English", RC_type == "Subject_RC"),
                                 REML = TRUE)
summary(lme_r4_total_eng_subj_rc)

lme_r4_total_eng_obj_rc <- lmer(log_R4_Total_Time ~ Condition + r4_n_words +
                                  (1|Participant) + (1|TOI),
                                data = filter(df_r4_rc, Language == "English", RC_type == "Object_RC"),
                                REML = TRUE)
summary(lme_r4_total_eng_obj_rc)

# Extract results
r3_fp_rc_results <- rbind(
  extract_lme(lme_r3_fp_cat_subj_rc, "Catalan Subject RC"),
  extract_lme(lme_r3_fp_cat_obj_rc, "Catalan Object RC"),
  extract_lme(lme_r3_fp_eng_subj_rc, "English Subject RC"),
  extract_lme(lme_r3_fp_eng_obj_rc, "English Object RC")
)
View(r3_fp_rc_results)

r3_total_rc_results <- rbind(
  extract_lme(lme_r3_total_cat_subj_rc, "Catalan Subject RC"),
  extract_lme(lme_r3_total_cat_obj_rc, "Catalan Object RC"),
  extract_lme(lme_r3_total_eng_subj_rc, "English Subject RC"),
  extract_lme(lme_r3_total_eng_obj_rc, "English Object RC")
)
View(r3_total_rc_results)

r4_fp_rc_results <- rbind(
  extract_lme(lme_r4_fp_cat_subj_rc, "Catalan Subject RC"),
  extract_lme(lme_r4_fp_cat_obj_rc, "Catalan Object RC"),
  extract_lme(lme_r4_fp_eng_subj_rc, "English Subject RC"),
  extract_lme(lme_r4_fp_eng_obj_rc, "English Object RC")
)
View(r4_fp_rc_results)

r4_total_rc_results <- rbind(
  extract_lme(lme_r4_total_cat_subj_rc, "Catalan Subject RC"),
  extract_lme(lme_r4_total_cat_obj_rc, "Catalan Object RC"),
  extract_lme(lme_r4_total_eng_subj_rc, "English Subject RC"),
  extract_lme(lme_r4_total_eng_obj_rc, "English Object RC")
)
View(r4_total_rc_results)

# Comprehension Questions to check participants choice preference
# Load ambiguous responses
ambiguous_responses <- read_excel ("ambiguous_responses.xlsx", sheet = "RC")

# Summary by RC_type and language

rc_type_summary <- ambiguous_responses %>%
  group_by(Language, RC_type) %>%
  summarise(
    mean_HA = round(mean(`HA %`), 1),
    mean_LA = round(mean(`LA %`), 1),
    .groups = "drop"
  )

View(rc_type_summary)

# check accuracy of control questions
# Load control accuracy data


# Summary of accuracy by RC_type and language
rc_accuracy_summary <- control_accuracy_rc %>%
  filter(!is.na(RC_type)) %>%
  group_by(RC_type, Group) %>%
  summarise(
    mean_accuracy = round(mean(Accuracy), 1),
    .groups = "drop"
  )

View(rc_accuracy_summary)


