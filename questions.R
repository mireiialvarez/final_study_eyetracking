setwd (dirname(rstudioapi::getActiveDocumentContext()$path))

library (rstudioapi)

getwd()

# read the columns of the tsv
data <- read.delim ("questions_export.tsv")
names (data)
# Check what we have in the Presented.Media.name and Event columns
unique (data$Presented.Media.name)
unique (data$Event)

# Filter only keyboard events (to see if the letters from the keyboard appear)
keyboard_events <- data[data$Event == "KeyboardEvent", ]

# View them
View(keyboard_events)


library(dplyr)

# Separate stimulus rows (have media name) and keyboard events
stimulus_intervals <- data %>%
  filter(Presented.Media.name != "" & !is.na(Presented.Media.name)) %>%
  select(Recording.timestamp, Presented.Media.name, Presented.Stimulus.name)

# Get keyboard events
keyboard_events <- data %>%
  filter(Event == "KeyboardEvent")

# Fill stimulus name by nearest preceding timestamp
install.packages("data.table")
library(data.table)
setDT(data)

# Forward fill the stimulus name
data[, Presented.Media.name.filled := nafill(
  fifelse(Presented.Media.name == "", NA_character_, Presented.Media.name),
  type = "locf"
)]

library(dplyr)
library(tidyr)

keyboard_filled <- data %>%
  mutate(Presented.Stimulus.name = na_if(Presented.Stimulus.name, ""),
         Participant.name = na_if(Participant.name, "")) %>%
  fill(Presented.Stimulus.name, Participant.name, .direction = "down") %>%
  filter(Event == "KeyboardEvent") %>%
  select(Participant.name, Event.value, Presented.Stimulus.name)

View(keyboard_filled)
# Filter out everything unless the keyboard event with "s" or "l"
keyboard_filled <- keyboard_filled %>%
  filter(Event.value %in% c("s", "l"))

View(keyboard_filled)

# s = L / l = R; change the letters to match it with the text 
keyboard_filled <- keyboard_filled %>%
  mutate(Event.value = recode(Event.value, "s" = "L", "l" = "R"))

View(keyboard_filled)

# Remove the filler questions, since they are of no interest
keyboard_filled <- keyboard_filled %>%
  filter(!grepl("^qfill", Presented.Stimulus.name))

View(keyboard_filled)

# Check how many answers x participants there are 
keyboard_filled %>%
  count(Participant.name)

# Count the number of L and Rs answers for each stimuli. Delete some remaining stimuli that are not questions and remove qctrpp2_eng as it wasn't the stimuli they saw. 
keyboard_filled <- keyboard_filled %>%
  filter(Presented.Stimulus.name != "qctrpp2_eng")

response_counts <- keyboard_filled %>%
  filter(grepl("^q", Presented.Stimulus.name)) %>%
  count(Presented.Stimulus.name, Event.value) %>%
  pivot_wider(names_from = Event.value, values_from = n, values_fill = 0)

View(response_counts)

# Save this as an excel to add the meaning of each response since they differ depending on the stimuli. 
install.packages("writexl")
library(writexl)
write_xlsx(response_counts, "response_counts.xlsx")

#Upload the xl again with the meanings added. 
library(readxl)
response_counts_labeled <- read_excel("response_counts.xlsx")

View(response_counts_labeled)

# Calculate accuracy of control sentences x language and type of ambiguity 
control_accuracy <- response_counts_labeled %>%
  filter(Type == "Control") %>%
  mutate(
    correct_n   = ifelse(Correct_ctrl_answer == "L", L, R),
    total_n     = L + R,
    pct_correct = correct_n / total_n * 100
  ) %>%
  group_by(Condition, Language) %>%
  summarise(mean_accuracy = mean(pct_correct), .groups = "drop")

View(control_accuracy)

# Plot accuracy results
library(ggplot2)
# Load fonts
library(extrafont)
loadfonts(device = "win")
fonts ()

# Fix group names 
control_accuracy <- control_accuracy %>%
  mutate(Group = paste0(Condition, " ", Language))

# Plot it
p_accuracy <- ggplot(control_accuracy, aes(x = Group, y = mean_accuracy, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Group",
    y = "Accuracy (%)",
    fill = "",
    caption = "Figure X. Mean accuracy per group."
  ) +
  theme(
    text = element_text(family = "Times New Roman", size = 10),
    axis.title = element_text(family = "Times New Roman", size = 10),
    axis.text = element_text(family = "Times New Roman", size = 9),
    plot.caption = element_text(hjust = 0, size = 9, family = "Times New Roman"),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank()
  )

p_accuracy

ggsave("FigureX_Control_Accuracy_Groups.png", p_accuracy,
       width = 18, height = 10, units = "cm", dpi = 300)

# Calculate overall accuracy
overall_accuracy <- response_counts_labeled %>%
  filter(Type == "Control") %>%
  mutate(
    correct_n   = ifelse(Correct_ctrl_answer == "L", L, R),
    total_n     = L + R,
    pct_correct = correct_n / total_n * 100
  ) %>%
  summarise(mean_accuracy = round(mean(pct_correct), 1))

overall_accuracy



# Check the % of accuracy x stimuli, so I can point out the one's with the lowest accuracy for control sentences. 
control_items <- response_counts_labeled %>%
  filter(Type == "Control") %>%
  mutate(
    correct_n   = ifelse(Correct_ctrl_answer == "L", L, R),
    total_n     = L + R,
    pct_correct = round(correct_n / total_n * 100, 1),
    Group       = paste(Condition, Language, sep = "_")
  ) %>%
  select(Group, Presented.Stimulus.name, pct_correct) %>%
  rename(
    Stimulus = Presented.Stimulus.name,
    Accuracy = pct_correct
  ) %>%
  arrange(factor(Group, levels = c("PP_cat", "PP_eng", "RC_cat", "RC_eng")))

View(control_items)

write_xlsx(control_items, "control_accuracy_appendix.xlsx")

# Creating two tables: one for PP and the other for RC for ambiguous sentences. 
# Calculate % of participants whose chose NP vs VP or HA vs LA regardless of which key (L/R) corresponded to each reading in that sentence. 
# PP table: columns = Stimulus, Language, NP att. %, VP att. %
pp_ambiguous <- response_counts_labeled %>%
  filter(Type == "Ambiguous", Condition == "PP") %>%
  mutate(
    total_n = L + R,
    NP_att  = round(ifelse(L_meaning == "NP att.", L, R) / total_n * 100, 1),
    VP_att  = round(ifelse(L_meaning == "VP att.", L, R) / total_n * 100, 1)
  ) %>%
  select(Presented.Stimulus.name, Language, NP_att, VP_att) %>%
  rename(Stimulus = Presented.Stimulus.name,
         "NP att. %" = NP_att,
         "VP att. %" = VP_att) %>%
  arrange(Language)

View(pp_ambiguous)

# RC table: columns = Stimulus, Language, HA %, LA %
rc_ambiguous <- response_counts_labeled %>%
  filter(Type == "Ambiguous", Condition == "RC") %>%
  mutate(
    total_n = L + R,
    HA      = round(ifelse(L_meaning == "HA", L, R) / total_n * 100, 1),
    LA      = round(ifelse(L_meaning == "LA", L, R) / total_n * 100, 1)
  ) %>%
  select(Presented.Stimulus.name, Language, HA, LA) %>%
  rename(Stimulus = Presented.Stimulus.name,
         "HA %" = HA,
         "LA %" = LA) %>%
  arrange(Language)

View(rc_ambiguous)

# Export both to Excel as separate sheets
write_xlsx(list(PP = pp_ambiguous, RC = rc_ambiguous), "ambiguous_responses.xlsx")

# PP: overall preference NP vs VP
pp_summary <- pp_ambiguous %>%
  summarise(
    mean_NP = round(mean(`NP att. %`), 1),
    mean_VP = round(mean(`VP att. %`), 1)
  )

# Split by language
pp_summary_lang <- pp_ambiguous %>%
  group_by(Language) %>%
  summarise(
    mean_NP = round(mean(`NP att. %`), 1),
    mean_VP = round(mean(`VP att. %`), 1)
  )

# RC: overall preference HA vs LA
rc_summary <- rc_ambiguous %>%
  summarise(
    mean_HA = round(mean(`HA %`), 1),
    mean_LA = round(mean(`LA %`), 1)
  )

# Split by language
rc_summary_lang <- rc_ambiguous %>%
  group_by(Language) %>%
  summarise(
    mean_HA = round(mean(`HA %`), 1),
    mean_LA = round(mean(`LA %`), 1)
  )

pp_summary
pp_summary_lang
rc_summary
rc_summary_lang