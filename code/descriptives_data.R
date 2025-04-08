# Thesis Scherer – Deskriptives
# -----------------------------
# Figures 2 to 5: Opinion Trends, Government Speech Distribution, Runoff Variable, Cosine Similarity

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(arrow)
library(showtext)
library(gridExtra)

# Set font
font_add("CMU Serif", "C:/Users/phili/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf")
showtext_auto()


# =========================================
# Figure 2: Most important issues over time
# =========================================

# Load opinion data
opinion_data <- read_parquet("Finale Datensätze/MostImportantIssueWeekly.parquet")

# Clean and format categories
opinion_data <- opinion_data %>%
  mutate(category = if_else(str_detect(issue, regex("corona", ignore_case = TRUE)), 
                            "3 - Health", category)) %>%
  mutate(
    percentage = as.numeric(percentage),
    category_num = as.numeric(str_extract(category, "^\\d+\\.?\\d*")),
    category_clean = str_remove(category, "^\\d+\\.?\\d*\\s-\\s")
  ) %>%
  arrange(category_num) %>%
  complete(date_opinion = seq(min(date_opinion), max(date_opinion), by = "week"), 
           category_clean, fill = list(percentage = 0)) %>%
  group_by(date_opinion, category_clean) %>%
  summarise(
    percentage = case_when(
      all(percentage == 0) ~ 0,
      sum(percentage > 0) == 1 ~ max(percentage),
      sum(percentage > 0) > 1 ~ mean(percentage[percentage > 0])
    ),
    .groups = "drop"
  )

opinion_data$category_clean <- factor(opinion_data$category_clean, levels = unique(opinion_data$category_clean))

# Generate gradient color fill for the categories
category_colors <- colorRampPalette(c("black", "white"))(length(unique(data_opinion$category_clean)))


# Create the plot
ggplot(data_opinion, aes(x = date_opinion, y = percentage, group = category_clean)) +
  geom_ribbon(aes(ymin = 0, ymax = percentage, fill = category_clean), alpha = 0.3) +  # Gradient fill effect
  geom_line(color = "black", size = 0.7) +  # Thinner lines
  facet_wrap(~ category_clean, ncol = 1, scales = "fixed", strip.position = "left") +  # Fixed scale
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 100)) +  # Force 0-100% scale for all
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Display every year on x-axis
  scale_fill_manual(values = category_colors) +  # Assign gradient colors
  labs(title = NULL,
       subtitle = NULL,
       x = NULL,
       y = NULL) + # Remove y-axis title
  theme_minimal(base_family = "CMU Serif") +  # Apply CMU Serif font
  theme(
    strip.text.y.left = element_text(size = 10, face = "plain", angle = 0, hjust = 0, vjust = -0.),  # Ensure labels align properly
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Keep x-axis labels straight
    axis.text.y = element_blank(),  # Remove default y-axis labels
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor = element_blank(),
    legend.position = "none",  # Hide legend
    strip.placement = "outside",  # Ensure category labels are outside of the panel
    plot.title = element_blank(),  # Remove title
    plot.subtitle = element_blank(),  # Remove subtitle
    plot.caption = element_blank(),  # Remove caption
    panel.spacing.y = unit(-1.5, "lines")  # Reduce space between facets for better alignment
  )




# ==========================================
# Figure 3: Government speech descriptives
# ==========================================

# Load speech data
start_date <- "2015-01-01"
end_date <- "2021-12-31"

speech_data <- read_rds("Finale Datensätze/GovernmentSpeech.rds") %>%
  filter(date >= start_date & date <= end_date) %>%
  mutate(date = as.Date(date),
         speech_id = paste0("P", row_number()))

# Yearly distribution
plot_year <- speech_data %>%
  mutate(year = year(date)) %>%
  count(year) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  ggplot(aes(x = factor(year), y = percentage)) +
  geom_bar(stat = "identity", fill = "#B2B2B2") +
  labs(title = "Years") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.title = element_blank())

# Monthly distribution
plot_month <- speech_data %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  count(month) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  ggplot(aes(x = month, y = percentage)) +
  geom_bar(stat = "identity", fill = "#B2B2B2") +
  labs(title = "Months") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.title = element_blank())

# Weekly distribution
plot_weekday <- speech_data %>%
  mutate(weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 1)) %>%
  count(weekday) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  ggplot(aes(x = weekday, y = percentage)) +
  geom_bar(stat = "identity", fill = "#B2B2B2") +
  labs(title = "Week Days") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.title = element_blank())

# Type distribution
plot_type <- speech_data %>%
  filter(!type %in% c("briefing", "Other")) %>%
  mutate(type = tolower(type),
         type = sub("^\\S+ ", "", type)) %>%
  count(type) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  ggplot(aes(x = reorder(type, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#B2B2B2") +
  labs(title = "Speech Type") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.title = element_blank())

# Combine and save plots
grid_arranged <- grid.arrange(plot_year, plot_month, plot_weekday, plot_type, ncol = 2)


# ==================================================
# Figure 4: Histogram of the runoff variable
# ==================================================

merged_data_full <- read_rds("V2_cosine_full.rds")

runoff_data <- merged_data_full %>%
  mutate(distance_days = as.numeric(distance_days)) %>%
  filter(distance_days > -80 & distance_days < 80) %>%
  group_by(weekday, distance_days) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(weekday) %>%
  mutate(prop = n / sum(n))

ggplot(runoff_data, aes(x = distance_days, y = prop)) +
  geom_col(fill = "black", color = "black", alpha = 0.8) +
  labs(x = "Distance Days", y = "Proportion by Weekday") +
  theme_minimal()


# ==================================================
# Figure 5: Daily average cosine similarity
# ==================================================

cosine_data <- merged_data_full %>%
  mutate(percentage = as.numeric(percentage)) %>%
  filter(category_opinion == category_speech) %>% 
  filter(distance_days > -20 & distance_days < 20) %>%
  group_by(distance_days) %>%
  summarise(
    mean_cosine = weighted.mean(cosine_similarity, w = percentage, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(cosine_data, aes(x = distance_days, y = mean_cosine)) +
  geom_point(color = "gray70", size = 1, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "gray80", span = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "CMU Serif")
