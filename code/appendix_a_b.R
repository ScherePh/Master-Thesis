# Thesis Scherer – Appendix Analysis
# =======================================
# Appendix sections: Word-level Contributions, Correlation Matrix, Subgroup Cosine Trends

# Load Required Libraries
library(tidyverse)
library(lubridate)
library(tidytext)
library(stringr)
library(ggplot2)
library(showtext)
library(ggforce)
library(forcats)
library(corrplot)
library(rdrobust)

# Font
font_add("CMU Serif", "C:/Users/phili/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf")
showtext_auto()

# ======================================
# Word level contributions
# ======================================
rdd_sample <- rdd_data %>%
  filter(distance_days >= -10 & distance_days <= 10, category_opinion == category_speech)

tidy_speech <- rdd_sample %>% select(speech_id, speech_cleaned) %>% distinct() %>% unnest_tokens(word, speech_cleaned)
tidy_keywords <- rdd_sample %>% select(opinion_id, keywords_cleaned) %>% distinct() %>% unnest_tokens(word, keywords_cleaned)

speech_counts <- tidy_speech %>% count(speech_id, word, name = "n_speech")
keyword_counts <- tidy_keywords %>% count(opinion_id, word, name = "n_report")

doc_pairs <- rdd_sample %>% select(speech_id, opinion_id) %>% distinct()

combined_counts <- doc_pairs %>%
  left_join(speech_counts, by = "speech_id") %>%
  left_join(keyword_counts, by = c("opinion_id", "word")) %>%
  replace_na(list(n_speech = 0, n_report = 0))

speech_norms <- speech_counts %>% group_by(speech_id) %>% summarise(norm_speech = sum(n_speech^2), .groups = "drop")
dot_products <- combined_counts %>% mutate(prod = n_speech * n_report) %>%
  group_by(speech_id, opinion_id) %>% summarise(dot = sum(prod), .groups = "drop")

wk_df <- combined_counts %>%
  left_join(speech_norms, by = "speech_id") %>%
  left_join(dot_products, by = c("speech_id", "opinion_id")) %>%
  filter(dot != 0 & norm_speech != 0) %>%
  mutate(wk = (n_speech * n_report / dot) - (n_speech^2 / norm_speech)) %>%
  select(speech_id, opinion_id, word, wk)

wk_df_cat <- wk_df %>%
  left_join(rdd_sample %>% select(speech_id, category_opinion), by = "speech_id") %>%
  rename(category = category_opinion) %>%
  filter(!is.na(category))

plot_data <- wk_df_cat %>%
  group_by(category, word) %>%
  summarise(mean_wk = mean(wk, na.rm = TRUE), .groups = "drop") %>%
  mutate(direction = ifelse(mean_wk > 0, "+", "-")) %>%
  group_by(category) %>%
  slice_max(order_by = abs(mean_wk), n = 10) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(word = reorder(word, mean_wk)) %>%
  ungroup()

plot_data_clean <- plot_data %>%
  mutate(
    category = str_remove(category, "^\\d+(\\.\\d+)?\\s*-\\s*"),
    direction_label = ifelse(mean_wk > 0, "+", "-"),
    mean_wk_abs = abs(mean_wk)
  ) %>%
  filter(category != "Non-thematic") %>%
  group_by(category) %>%
  slice_max(order_by = mean_wk_abs, n = 8) %>%
  ungroup()

n_pages <- 2
for (i in 1:n_pages) {
  p <- ggplot(plot_data_clean, aes(x = mean_wk_abs, y = fct_rev(word))) +
    geom_text(aes(label = direction_label), hjust = -0.3, size = 5, fontface = "bold", color = "black") +
    facet_wrap_paginate(~ category, scales = "free_y", ncol = 2, nrow = 5, page = i) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    theme_minimal(base_family = "CMU Serif") +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 13, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.text.x = element_text(size = 12, color = "black"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
      panel.grid = element_blank()
    )
  print(p)
}

# ======================================
# Correlation Matrix
# ======================================
selected_vars <- merged_data1 %>%
  select(weekday, month, year, election_year, type, legislative_period, cdu_supporter, issue_own)

selected_vars_numeric <- selected_vars %>%
  mutate(
    weekday = as.numeric(weekday),
    type = as.numeric(as.factor(type)),
    cdu_supporter = as.numeric(cdu_supporter),
    issue_own = as.numeric(issue_own),
    election_year = as.numeric(election_year),
    month = as.numeric(month),
    year = as.numeric(year),
    legislative_period = as.numeric(legislative_period)
  ) %>%
  na.omit()

cor_matrix <- cor(selected_vars_numeric)
par(family = "CMU Serif")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)
par(family = "")

# ======================================
# Cosine similarity per subgroup
# ======================================
# --- by Type ---
data_binned_type <- rdd_data_cosine %>%
  filter(category_opinion == category_speech) %>%
  mutate(distance_days = as.numeric(distance_days)) %>%
  filter(distance_days >= -20 & distance_days <= 20) %>%
  group_by(type, distance_days) %>%
  summarise(mean_cosine = weighted.mean(cosine_similarity, w = percentage, na.rm = TRUE), .groups = "drop")

ggplot(data_binned_type, aes(x = distance_days, y = mean_cosine)) +
  geom_point(color = "gray70", size = 1, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "gray80", span = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ type) +
  theme_minimal(base_family = "CMU Serif")

# --- by Issue ---
data_binned_issue <- rdd_data_cosine %>%
  filter(category_opinion == category_speech) %>%
  mutate(distance_days = as.numeric(distance_days), category_opinion = str_to_lower(category_opinion)) %>%
  filter(distance_days >= -20 & distance_days <= 20) %>%
  group_by(distance_days, category_opinion) %>%
  summarise(mean_cosine = weighted.mean(cosine_similarity, w = percentage, na.rm = TRUE), .groups = "drop")

ggplot(data_binned_issue, aes(x = distance_days, y = mean_cosine)) +
  geom_point(color = "gray70", size = 1, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "gray80", span = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ category_opinion, scales = "free_y", ncol = 4) +
  theme_minimal(base_family = "CMU Serif") +
  theme(strip.text = element_text(face = "bold"), panel.grid = element_blank())

# --- by Publisher ---
data_binned_publisher <- rdd_data_cosine %>%
  filter(category_opinion == category_speech, publisher != "Other") %>%
  mutate(distance_days = as.numeric(distance_days), publisher = str_to_lower(publisher)) %>%
  filter(distance_days >= -20 & distance_days <= 20) %>%
  group_by(distance_days, publisher) %>%
  summarise(mean_cosine = weighted.mean(cosine_similarity, w = percentage, na.rm = TRUE), .groups = "drop")

ggplot(data_binned_publisher, aes(x = distance_days, y = mean_cosine)) +
  geom_point(color = "gray70", size = 1, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "gray80", span = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ publisher, scales = "free_y") +
  theme_minimal(base_family = "CMU Serif") +
  theme(strip.text = element_text(face = "bold"), panel.grid = element_blank())
