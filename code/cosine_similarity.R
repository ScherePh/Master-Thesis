# Thesis Scherer – Construction of Cosine Similarity Measurement
# ==============================================================
# This script computes cosine similarity between government speech 
# (per paragraph) and top public issues, based on TF-IDF vectorization.

# Load Required Libraries
library(tidyverse)
library(lubridate)
library(text2vec)
library(textstem)
library(tidytext)
library(tm)
library(SnowballC)
library(Matrix)
library(arrow)
library(rdd)
library(rdrobust)
library(ggplot2)
library(gridExtra)

# ===================================
# 1. Load and Prepare Data
# ===================================

# Load weekly public issue data
data_opinion <- read_parquet("Finale Datensätze/MostImportantIssueWeekly.parquet") %>% 
  filter(topic_new == 1) %>% 
  mutate(opinion_id = paste0("R", row_number()))

# Define date window around reports
start_date <- min(data_opinion$date_opinion) - 120
end_date <- max(data_opinion$date_opinion) + 120

# Load government speech data (paragraph-level)
data_speech <- read_parquet("Finale Datensätze/Classified_GovernmentSpeech_per_paragraph1.parquet") %>% 
  filter(date >= start_date & date <= end_date) %>% 
  mutate(date = as.Date(date),
         speech_id = paste0("P", row_number()))

# ===================================
# 2. Compute Cosine Similarity
# ===================================

# Combine text from public opinion and speech
all_texts <- c(data_opinion$clean_text, data_speech$clean_text)

# Tokenize and construct TF-IDF matrix
tokens <- word_tokenizer(all_texts)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm, tfidf)

# Split into opinion and speech matrices
num_opinions <- nrow(data_opinion)
num_speeches <- nrow(data_speech)

dtm_opinion <- dtm_tfidf[1:num_opinions, ]
dtm_speech  <- dtm_tfidf[(num_opinions + 1):(num_opinions + num_speeches), ]

# Compute cosine similarity matrix
cosine_sim_matrix <- as.matrix(sim2(dtm_opinion, dtm_speech, method = "cosine"))

# ===================================
# 3. Merge Similarity Results
# ===================================

# Convert to long format
results <- expand.grid(
  opinion_id = data_opinion$opinion_id, 
  speech_id  = data_speech$speech_id
) %>%
  mutate(cosine_similarity = as.vector(cosine_sim_matrix))

# Merge metadata
final_results <- results %>%
  full_join(data_opinion, by = "opinion_id") %>%
  full_join(data_speech,  by = "speech_id") %>%
  mutate(distance_days = as.numeric(date - date_opinion))

# ===================================
# 4. Create Final RDD Dataset
# ===================================

rdd_data_full <- final_results %>% 
  select(opinion_id, date_opinion, category, issue, percentage, clean_text.x, issue_own, 
         cdu_supporter, cdu_percentage, speech_id, date, type, publisher, Category, title, 
         clean_text.y, distance_days, cosine_similarity) %>% 
  rename(
    category_opinion = category,
    category_speech  = Category,
    keywords_cleaned = clean_text.x,
    speech_cleaned   = clean_text.y,
    date_speech      = date
  ) %>%
  mutate(
    weekday = wday(date_speech, label = TRUE, abbr = FALSE, locale = "en_US"),
    month = month(date_speech, label = TRUE, abbr = FALSE, locale = "en_US"),
    year = year(date_speech),
    election_year = ifelse(year %in% c(2017, 2021), 1, 0),
    legislative_period = case_when(
      year >= 2013 & year <= 2017 ~ 18,
      year >= 2017 & year <= 2021 ~ 19,
      year >= 2021 ~ 20,
      TRUE ~ NA_real_
    ),
    weekend = ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0)
  )

# Keep only ±120 day window
rdd_data_filtered <- rdd_data_full %>%
  filter(distance_days >= -120 & distance_days <= 120)

# ===================================
# 5. Save Output
# ===================================

write_rds(rdd_data_full, "V2d_cosine_full.rds")
write_rds(rdd_data_filtered, "V2d_cosine_filtered.rds")

