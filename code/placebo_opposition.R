# Thesis Scherer – Opposition Press Releases
# ================================================
# RDD models based on cosine similarity between opposition press releases and public priorities.
# Note: Includes fixed bandwidth (h = 5), with and without covariates.

# ---------------------------- #
# Load Required Libraries
# ---------------------------- #
library(tidyverse)
library(lubridate)
library(rdrobust)
library(arrow)
library(text2vec)
library(Matrix)
library(SnowballC)
library(showtext)

# Font for plots
font_add("CMU Serif", "C:/Users/phili/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf")
showtext_auto()

# ---------------------------- #
# 1. Load and Prepare Data
# ---------------------------- #

data_opinion <- read_parquet("Finale Datensätze/MostImportantIssueWeekly.parquet") %>% 
  filter(topic_new == 1) %>% 
  mutate(opinion_id = paste0("R", row_number()))

data_texts <- read_rds("partypress_texts.rds")
data_meta <- read_rds("partypress.rds")

press_data <- data_meta %>%
  full_join(data_texts, by = c("id", "country_name")) %>%
  filter(country_name == "germany", !party %in% c("CDU/CSU", "SPD")) %>%
  mutate(
    date = as.Date(date),
    speech_id = paste0("P", row_number()),
    weekend_dummy = ifelse(wday(date) %in% c(1, 7), 1, 0)
  )

# ---------------------------- #
# 2. Clean Texts & Compute Similarity
# ---------------------------- #

clean_text <- function(text) {
  text %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    word_tokenizer() %>%
    unlist() %>%
    removeWords(stopwords("de")) %>%
    wordStem(language = "de") %>%
    paste(collapse = " ")
}

press_data <- press_data %>%
  mutate(clean_text = map_chr(text, clean_text))

all_texts <- c(data_opinion$clean_text, press_data$clean_text)
tokens <- word_tokenizer(all_texts)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)
tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm, tfidf)

n_opinions <- nrow(data_opinion)
dtm_opinion <- dtm_tfidf[1:n_opinions, ]
dtm_press <- dtm_tfidf[(n_opinions + 1):nrow(dtm_tfidf), ]

sim_matrix <- as.matrix(sim2(dtm_opinion, dtm_press, method = "cosine"))

# ---------------------------- #
# 3. Merge and Prepare for RDD
# ---------------------------- #
data_opinion <- data_opinion %>%
  mutate(
    category_code = str_extract(category, "^\\d+\\.?\\d*"),  # Zahl extrahieren
    category_code = str_replace(category_code, "\\.", ""),   # Dezimalpunkt entfernen
    category_code = as.integer(category_code)                # In Integer umwandeln
  )

merged_data <- expand.grid(opinion_id = data_opinion$opinion_id,
                           speech_id = press_data$speech_id) %>% 
  mutate(cosine_similarity = as.vector(sim_matrix)) %>%
  left_join(data_opinion, by = "opinion_id") %>%
  left_join(press_data, by = "speech_id") %>%
  mutate(distance_days = as.numeric(date - date_opinion))

rdd_data <- merged_data %>%
  filter(distance_days >= -40 & distance_days <= 40) %>%
  filter(category_code == issue_mono) %>% 
  select(distance_days, percentage, cosine_similarity, weekend_dummy, date, party) %>%
  mutate(
    weekday = as.numeric(wday(date)),
    month = as.numeric(month(date)),
    year = as.numeric(year(date)),
    election_year = ifelse(year %in% c(2017, 2021), 1, 0),
    legislative_period = case_when(
      year >= 2013 & year <= 2017 ~ 18,
      year >= 2017 & year <= 2021 ~ 19,
      year >= 2021 ~ 20,
      TRUE ~ NA_real_
    ),
    party_numeric = as.numeric(as.factor(party)),
    percentage = as.numeric(percentage)
  )

control_vars <- c("weekday", "month", "year", "election_year", "legislative_period", "party_numeric")
control_vars <- control_vars[control_vars %in% colnames(rdd_data)]

# ---------------------------- #
# 4. Run RDD Models
# ---------------------------- #

extract_rdd_results <- function(model) {
  data.frame(
    Coef = model$coef["Conventional", "Coeff"],
    SE = model$se["Conventional", "Std. Err."],
    P_Value = model$pv["Conventional", "P>|z|"],
    Robust_Coef = model$coef["Robust", "Coeff"],
    Robust_SE = model$se["Robust", "Std. Err."],
    Robust_P_Value = model$pv["Robust", "P>|z|"],
    N = model$N[1]
  )
}

bw <- 5

model_opposition <- list(
  "No Cov" = rdrobust(
    y = rdd_data$cosine_similarity,
    x = rdd_data$distance_days,
    c = 0,
    h = bw,
    weights = rdd_data$percentage
  ),
  "With Cov" = rdrobust(
    y = rdd_data$cosine_similarity,
    x = rdd_data$distance_days,
    c = 0,
    h = bw,
    weights = rdd_data$percentage,
    covs = as.matrix(rdd_data %>% select(all_of(control_vars)))
  )
)

results_opposition <- bind_rows(
  lapply(model_opposition, extract_rdd_results),
  .id = "Covariates"
)

# ---------------------------- #
# 5. Save Results
# ---------------------------- #

if (!dir.exists("estimates")) dir.create("estimates")
write_rds(results_opposition, "estimates/rdd_cosine_opposition_pressrelease.rds")
