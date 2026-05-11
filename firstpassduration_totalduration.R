# Set working directory to the script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# install the needed packages for the code
#install.packages("readxl") 
library(readxl)
library(dplyr)
library (nlme)
library (ggplot2)
library (lattice)
library (performance)
library(stringr)
library(ggplot2)
library(patchwork)

df <- read_excel("tobii_metrics.xlsx")
View (df)

# Categorizing sentences by condition, language and structure type.  
df_final_columns <- df %>%
  mutate(
    # Condition: ctr = Control, otherwise Ambiguous. 
    Condition = ifelse(str_detect(TOI, "ctr"), "Control", "Ambiguous"),
    # Language: "cat" = Catalan,"eng"= English
    Language = case_when(
      str_detect(TOI, "cat") ~ "Catalan",
      str_detect(TOI, "eng") ~ "English",
      TRUE ~ "Other"
    ),
    
    # Structure Type: "pp" = PP, rc = RC
    Type = case_when(
      str_detect(TOI, "pp") ~ "PP",
      str_detect(TOI, "rc") ~ "RC",
      TRUE ~ "Filler"
    )
  )

View(df_final_columns)

# Calculating metrics for R3
df_r3 <- df_final_columns %>%
  # Groups data by participant and sentence, preserving key variables
  group_by(Participant, Language, Condition, Type, TOI) %>%
  summarize(
    R3_Total_Time = sum(Total_duration_of_fixations[Region == "r3"], na.rm = TRUE),
    R3_First_Pass = sum(Firstpass_duration[Region == "r3"], na.rm = TRUE),
    .groups = "drop"
  )%>%   
# Both 0 and NA indicate no valid fixation (skipped region).They are both treated as NA to not bias results. 
mutate(
  R3_First_Pass = replace(R3_First_Pass, R3_First_Pass == 0, NA),
  R3_Total_Time = replace(R3_Total_Time, R3_Total_Time == 0, NA)
)

View (df_r3)
# Number of words in each region calculated as max - min word index + 1
# e.g. word indices 6,7,8,9 = 9 - 6 + 1 = 4 words
r3_length <- df_final_columns %>%
  filter(Region == "r3") %>%
  group_by(TOI) %>%
  summarise(r3_n_words = max(Word_index) - min(Word_index) + 1, 
            .groups = "drop")
View (r3_length)
# Add region word count to df_r3
df_r3 <- df_r3 %>%
  left_join(r3_length, by = "TOI")

# Skip rate for R3 First-pass duration
r3_skip_rate <- df_r3 %>%
  group_by(Language, Type, Condition) %>%
  summarise(
    n_total = n(),
    n_skipped = sum(is.na(R3_First_Pass)),
    skip_rate = round((n_skipped / n_total) * 100, 1),
    .groups = "drop"
  )
View(r3_skip_rate)


# Calculating metrics for R4
df_r4 <- df_final_columns %>%
  # Groups data by participant and sentence, preserving key variables  
  group_by(Participant, Language, Condition, Type, TOI) %>%
  summarize(
    # Calculating first-pass duration and total durations by summing all of the word belonging to R3. 
    R4_Total_Time   = sum(Total_duration_of_fixations[Region == "r4"], na.rm = TRUE),
    R4_First_Pass   = sum(Firstpass_duration[Region == "r4"], na.rm = TRUE),
    .groups = "drop"
  )%>% 
mutate(
    R4_First_Pass = replace(R4_First_Pass, R4_First_Pass == 0, NA),
    R4_Total_Time = replace(R4_Total_Time, R4_Total_Time == 0, NA)
  )
  
View (df_r4)

# Number of words in each region calculated as max - min word index + 1
r4_length <- df_final_columns %>%
  filter(Region == "r4") %>%
  group_by(TOI) %>%
  summarise(r4_n_words = max(Word_index) - min(Word_index) + 1, 
            .groups = "drop")
View (r4_length)
# Add region word count to df_r4
df_r4 <- df_r4 %>%
  left_join(r4_length, by = "TOI")

# Skip rate for first pass duration in R4
r4_skip_rate <- df_r4 %>%
  group_by(Language, Type, Condition) %>%
  summarise(
    n_total = n(),
    n_skipped = sum(is.na(R4_First_Pass)),
    skip_rate = round((n_skipped / n_total) * 100, 1),
    .groups = "drop"
  )
View(r4_skip_rate)

# Descriptive statistics (calculating SE and means)
# R3 First-pass duration
r3_fp_summary <- df_r3 %>%
  filter(R3_First_Pass > 0) %>%
  group_by(Language, Type, Condition) %>%
  summarise(
    mean_fp = mean(R3_First_Pass, na.rm = TRUE),
    se_fp   = sd(R3_First_Pass, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# R3 Total fixation duration
r3_total_summary <- df_r3 %>%
  filter(R3_Total_Time > 0) %>%
  group_by(Language, Type, Condition) %>%
  summarise(
    mean_total = mean(R3_Total_Time, na.rm = TRUE),
    se_total   = sd(R3_Total_Time, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# R4 First-pass duration
r4_fp_summary <- df_r4 %>%
  filter(R4_First_Pass > 0) %>%
  group_by(Language, Type, Condition) %>%
  summarise(
    mean_fp = mean(R4_First_Pass, na.rm = TRUE),
    se_fp   = sd(R4_First_Pass, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# R4 Total fixation duration
r4_total_summary <- df_r4 %>%
  filter(R4_Total_Time > 0) %>%
  group_by(Language, Type, Condition) %>%
  summarise(
    mean_total = mean(R4_Total_Time, na.rm = TRUE),
    se_total   = sd(R4_Total_Time, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 4. Graphics (raw data) for each metric and region. 
# Load extrafont to use Times New Roman
#install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device = "win")

# Define colors for each condition (Control = blue, Ambiguous = red)
condition_colors <- c("Control" = "steelblue", "Ambiguous" = "firebrick")


# Updated make_plot function with Times New Roman
make_plot <- function(data, y_var, se_var, title, y_label, y_limits) {
  ggplot(data, aes(x = Type, y = .data[[y_var]], fill = Condition)) +
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

# Figure 1: R3 First-pass duration
y_max_fp <- max(r3_fp_summary$mean_fp + r3_fp_summary$se_fp) * 1.1

p1_cat <- make_plot(
  filter(r3_fp_summary, Language == "Catalan"),
  "mean_fp", "se_fp", "Catalan",
  "Mean First-pass Duration (ms)",
  c(0, y_max_fp)
)
p1_eng <- make_plot(
  filter(r3_fp_summary, Language == "English"),
  "mean_fp", "se_fp", "English",
  "Mean First-pass Duration (ms)",
  c(0, y_max_fp)
)

figure1 <- (p1_cat | p1_eng) +
  plot_annotation(
    caption = "Figure 1. Mean first-pass duration (ms) in R3 for PP and RC structures
in Catalan (left) and English (right). Error bars represent standard error.
Blue = Control, Red = Ambiguous.",
    theme = theme(
      plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

figure1

ggsave("Figure1_R3_Firstpass.png", figure1,
       width = 18, height = 10, units = "cm", dpi = 300)

# Figure 2: R3 Total Fixation Duration

y_max_r3_total <- max(r3_total_summary$mean_total + r3_total_summary$se_total) * 1.1

p2_cat <- make_plot(
  filter(r3_total_summary, Language == "Catalan"),
  "mean_total", "se_total", "Catalan",
  "Mean Total Fixation Duration (ms)",
  c(0, y_max_r3_total)
)
p2_eng <- make_plot(
  filter(r3_total_summary, Language == "English"),
  "mean_total", "se_total", "English",
  "Mean Total Fixation Duration (ms)",
  c(0, y_max_r3_total)
)

figure2 <- (p2_cat | p2_eng) +
  plot_annotation(
    caption = "Figure 2. Mean total fixation duration (ms) in R3 for PP and RC structures
in Catalan (left) and English (right). Error bars represent standard error.
Blue = Control, Red = Ambiguous.",
    theme = theme(
      plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

figure2

ggsave("Figure2_R3_TotalFixation.png", figure2,
       width = 18, height = 10, units = "cm", dpi = 300)

# Figure 3: R4 First-pass Duration

y_max_r4_fp <- max(r4_fp_summary$mean_fp + r4_fp_summary$se_fp) * 1.1

p3_cat <- make_plot(
  filter(r4_fp_summary, Language == "Catalan"),
  "mean_fp", "se_fp", "Catalan",
  "Mean First-pass Duration (ms)",
  c(0, y_max_r4_fp)
)
p3_eng <- make_plot(
  filter(r4_fp_summary, Language == "English"),
  "mean_fp", "se_fp", "English",
  "Mean First-pass Duration (ms)",
  c(0, y_max_r4_fp)
)

figure3 <- (p3_cat | p3_eng) +
  plot_annotation(
    caption = "Figure 3. Mean first-pass duration (ms) in R4 for PP and RC structures
in Catalan (left) and English (right). Error bars represent standard error.
Blue = Control, Red = Ambiguous.",
    theme = theme(
      plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

figure3

ggsave("Figure3_R4_Firstpass.png", figure3,
       width = 18, height = 10, units = "cm", dpi = 300)

# Figure 4: R4 Total Fixation Duration

y_max_r4_total <- max(r4_total_summary$mean_total + r4_total_summary$se_total) * 1.1

p4_cat <- make_plot(
  filter(r4_total_summary, Language == "Catalan"),
  "mean_total", "se_total", "Catalan",
  "Mean Total Fixation Duration (ms)",
  c(0, y_max_r4_total)
)
p4_eng <- make_plot(
  filter(r4_total_summary, Language == "English"),
  "mean_total", "se_total", "English",
  "Mean Total Fixation Duration (ms)",
  c(0, y_max_r4_total)
)

figure4 <- (p4_cat | p4_eng) +
  plot_annotation(
    caption = "Figure 4. Mean total fixation duration (ms) in R4 for PP and RC structures
in Catalan (left) and English (right). Error bars represent standard error.
Blue = Control, Red = Ambiguous.",
    theme = theme(
      plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

figure4

ggsave("Figure4_R4_TotalFixation.png", figure4,
       width = 18, height = 10, units = "cm", dpi = 300)

# Normalising the data before applying any statistics. 
#Log transformation is applied to approximate normality and reduce influence outliers.
# NAs are automatically preserved as log(NA) = NA.

# R3 log transformation
df_r3 <- df_r3 %>%
  mutate(
    log_R3_First_Pass = log(R3_First_Pass),
    log_R3_Total_Time = log(R3_Total_Time)
  )

# R4 log transformation
df_r4 <- df_r4 %>%
  mutate(
    log_R4_First_Pass = log(R4_First_Pass),
    log_R4_Total_Time = log(R4_Total_Time)
  )

# Checking normality of the log-transformed variables visually and statistically before running the LME models.
check_normality <- function(data, var, title) {
# Histogram:shows the shape of the distribution.
# After log transformation we expect a roughly bell-shaped distribution.
  p1 <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(title = paste("Histogram -", title), x = var, y = "Count") +
    theme_minimal()
# Q-Q plot: compares the data against a perfect distribution. 
# Points should follow the red diagonal line closely if data is normal.
  p2 <- ggplot(data, aes(sample = .data[[var]])) +
    stat_qq() +
    stat_qq_line(color = "firebrick") +
    labs(title = paste("Q-Q Plot -", title)) +
    theme_minimal()
  
  p1 | p2
}

# R3 normality check
check_normality(df_r3, "log_R3_First_Pass", "R3 First-pass")
check_normality(df_r3, "log_R3_Total_Time", "R3 Total Duration")

# R4 normality check 
check_normality(df_r4, "log_R4_First_Pass", "R4 First-pass")
check_normality(df_r4, "log_R4_Total_Time", "R4 Total Duration")

# Shapiro-Wilk test to check normality after log.
# Note: with large samples (n > 100) this test is overly sensitive and will often return p < .05 even for approximately normal data. 
#Visual inspection of the plots above should be prioritised over this test.
shapiro.test(na.omit(df_r3$log_R3_First_Pass))
shapiro.test(df_r3$log_R3_Total_Time |> na.omit())
shapiro.test(df_r4$log_R4_First_Pass |> na.omit())
shapiro.test(df_r4$log_R4_Total_Time |> na.omit())


library(lme4)
library(lmerTest)

# LME Models
# Condition is sum-coded so the intercept represents the grand mean and the estimate reflects the ambiguity effect directly
df_r3$Condition <- factor(df_r3$Condition)
contrasts(df_r3$Condition) <- c(-0.5, 0.5)  # Ambiguous = -0.5, Control = 0.5

df_r4$Condition <- factor(df_r4$Condition)
contrasts(df_r4$Condition) <- c(-0.5, 0.5)


# R3 First-pass duration
lme_r3_fp_cat_pp <- lmer(log_R3_First_Pass ~ Condition + r3_n_words +
                           (1|Participant) + (1|TOI),
                         data = filter(df_r3, Language == "Catalan", Type == "PP"),
                         REML = TRUE)
summary(lme_r3_fp_cat_pp)

lme_r3_fp_cat_rc <- lmer(log_R3_First_Pass ~ Condition + r3_n_words +
                           (1|Participant) + (1|TOI),
                         data = filter(df_r3, Language == "Catalan", Type == "RC"),
                         REML = TRUE)
summary(lme_r3_fp_cat_rc)

lme_r3_fp_eng_pp <- lmer(log_R3_First_Pass ~ Condition + r3_n_words +
                           (1|Participant) + (1|TOI),
                         data = filter(df_r3, Language == "English", Type == "PP"),
                         REML = TRUE)
summary(lme_r3_fp_eng_pp)

lme_r3_fp_eng_rc <- lmer(log_R3_First_Pass ~ Condition + r3_n_words +
                           (1|Participant) + (1|TOI),
                         data = filter(df_r3, Language == "English", Type == "RC"),
                         REML = TRUE)
summary(lme_r3_fp_eng_rc)

# R3 total duration
lme_r3_total_cat_pp <- lmer(log_R3_Total_Time ~ Condition + r3_n_words +
                              (1|Participant) + (1|TOI),
                            data = filter(df_r3, Language == "Catalan", Type == "PP"),
                            REML = TRUE)
summary(lme_r3_total_cat_pp)

lme_r3_total_cat_rc <- lmer(log_R3_Total_Time ~ Condition + r3_n_words +
                              (1|Participant) + (1|TOI),
                            data = filter(df_r3, Language == "Catalan", Type == "RC"),
                            REML = TRUE)
summary(lme_r3_total_cat_rc)

lme_r3_total_eng_pp <- lmer(log_R3_Total_Time ~ Condition + r3_n_words +
                              (1|Participant) + (1|TOI),
                            data = filter(df_r3, Language == "English", Type == "PP"),
                            REML = TRUE)
summary(lme_r3_total_eng_pp)

lme_r3_total_eng_rc <- lmer(log_R3_Total_Time ~ Condition + r3_n_words +
                              (1|Participant) + (1|TOI),
                            data = filter(df_r3, Language == "English", Type == "RC"),
                            REML = TRUE)
summary(lme_r3_total_eng_rc)


# R4 First-pass duration
lme_r4_fp_cat_pp <- lmer(log_R4_First_Pass ~ Condition + r4_n_words +
                           (1|Participant) + (1|TOI),
                         data = filter(df_r4, Language == "Catalan", Type == "PP"),
                         REML = TRUE)
summary(lme_r4_fp_cat_pp)

lme_r4_fp_cat_rc <- lmer(log_R4_First_Pass ~ Condition + r4_n_words +
                           (1|Participant) + (1|TOI),
                         data = filter(df_r4, Language == "Catalan", Type == "RC"),
                         REML = TRUE)
summary(lme_r4_fp_cat_rc)

lme_r4_fp_eng_pp <- lmer(log_R4_First_Pass ~ Condition + r4_n_words +
                           (1|Participant) + (1|TOI),
                         data = filter(df_r4, Language == "English", Type == "PP"),
                         REML = TRUE)
summary(lme_r4_fp_eng_pp)

lme_r4_fp_eng_rc <- lmer(log_R4_First_Pass ~ Condition + r4_n_words +
                           (1|Participant) + (1|TOI),
                         data = filter(df_r4, Language == "English", Type == "RC"),
                         REML = TRUE)
summary(lme_r4_fp_eng_rc)

# R4 Total fixation duration
lme_r4_total_cat_pp <- lmer(log_R4_Total_Time ~ Condition + r4_n_words +
                              (1|Participant) + (1|TOI),
                            data = filter(df_r4, Language == "Catalan", Type == "PP"),
                            REML = TRUE)
summary(lme_r4_total_cat_pp)

lme_r4_total_cat_rc <- lmer(log_R4_Total_Time ~ Condition + r4_n_words +
                              (1|Participant) + (1|TOI),
                            data = filter(df_r4, Language == "Catalan", Type == "RC"),
                            REML = TRUE)
summary(lme_r4_total_cat_rc)

lme_r4_total_eng_pp <- lmer(log_R4_Total_Time ~ Condition + r4_n_words +
                              (1|Participant) + (1|TOI),
                            data = filter(df_r4, Language == "English", Type == "PP"),
                            REML = TRUE)
summary(lme_r4_total_eng_pp)

lme_r4_total_eng_rc <- lmer(log_R4_Total_Time ~ Condition + r4_n_words +
                              (1|Participant) + (1|TOI),
                            data = filter(df_r4, Language == "English", Type == "RC"),
                            REML = TRUE)
summary(lme_r4_total_eng_rc)

# Function to extract key values from each model
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

# R3 First-pass results
r3_fp_results <- rbind(
  extract_lme(lme_r3_fp_cat_pp, "Catalan PP"),
  extract_lme(lme_r3_fp_cat_rc, "Catalan RC"),
  extract_lme(lme_r3_fp_eng_pp, "English PP"),
  extract_lme(lme_r3_fp_eng_rc, "English RC")
)
View(r3_fp_results)

# R3 Total fixation results
r3_total_results <- rbind(
  extract_lme(lme_r3_total_cat_pp, "Catalan PP"),
  extract_lme(lme_r3_total_cat_rc, "Catalan RC"),
  extract_lme(lme_r3_total_eng_pp, "English PP"),
  extract_lme(lme_r3_total_eng_rc, "English RC")
)
View(r3_total_results)

# R4 First-pass results
r4_fp_results <- rbind(
  extract_lme(lme_r4_fp_cat_pp, "Catalan PP"),
  extract_lme(lme_r4_fp_cat_rc, "Catalan RC"),
  extract_lme(lme_r4_fp_eng_pp, "English PP"),
  extract_lme(lme_r4_fp_eng_rc, "English RC")
)
View(r4_fp_results)

# R4 Total fixation results
r4_total_results <- rbind(
  extract_lme(lme_r4_total_cat_pp, "Catalan PP"),
  extract_lme(lme_r4_total_cat_rc, "Catalan RC"),
  extract_lme(lme_r4_total_eng_pp, "English PP"),
  extract_lme(lme_r4_total_eng_rc, "English RC")
)




