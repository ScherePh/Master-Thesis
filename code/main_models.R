# Thesis Scherer – Main Models (Cosine Similarity)
# ================================================
# RDD Models based on restricted sample on a 120 days window around the cutoff.
# Important: Computation is extensive. 
# Include sample_frac(0.1) if you want to test the code




# Load libraries
library(tidyverse)
library(lubridate)
library(rdrobust)
library(parallel)
library(showtext)

# Font for plots
font_add("CMU Serif", "C:/Users/phili/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf")
showtext_auto()

# Load data
data_cosine <- read_rds("V1data_cosine.rds") %>%
  mutate(
    date_speech = as.Date(date_speech),
    weekday = wday(date_speech, label = TRUE)
  ) %>%
  filter(distance_days >= -60 & distance_days <= 60) %>%
  na.omit() %>%
  mutate(
    weekday = as.numeric(factor(weekday, levels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"))),
    month = as.numeric(factor(month, levels = month.name))
  )

# Covariates
control_vars <- c("weekday", "month", "year", "election_year", "legislative_period")
control_vars <- control_vars[control_vars %in% colnames(data_cosine)]

# Extract RDD results
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

# Bandwidth
bw <- 5

# ===================================
# CENTRIST EXPECTATIONS
# ===================================

model_centrist <- list(
  "No Cov" = rdrobust(y = data_cosine$cosine_similarity,
                      x = data_cosine$distance_days,
                      c = 0, h = bw),
  "With Cov" = rdrobust(y = data_cosine$cosine_similarity,
                        x = data_cosine$distance_days,
                        c = 0, h = bw,
                        covs = as.matrix(data_cosine %>% select(all_of(control_vars))))
)

results_centrist <- bind_rows(
  lapply(model_centrist, extract_rdd_results),
  .id = "Covariates"
)

# ===================================
# PARTY SUPPORTER EXPECTATIONS
# ===================================

data_cdu_1 <- data_cosine %>% filter(cdu_supporter == 1)
data_cdu_0 <- data_cosine %>% filter(cdu_supporter == 0)

models_cdu <- bind_rows(
  bind_rows(
    list(
      "No Cov" = rdrobust(data_cdu_1$cosine_similarity, data_cdu_1$distance_days, c = 0, h = bw),
      "With Cov" = rdrobust(data_cdu_1$cosine_similarity, data_cdu_1$distance_days, c = 0, h = bw,
                            covs = as.matrix(data_cdu_1 %>% select(all_of(control_vars))))
    ) %>% lapply(extract_rdd_results),
    .id = "Covariates"
  ) %>% mutate(Subgroup = "CDU Support = 1"),
  
  bind_rows(
    list(
      "No Cov" = rdrobust(data_cdu_0$cosine_similarity, data_cdu_0$distance_days, c = 0, h = bw),
      "With Cov" = rdrobust(data_cdu_0$cosine_similarity, data_cdu_0$distance_days, c = 0, h = bw,
                            covs = as.matrix(data_cdu_0 %>% select(all_of(control_vars))))
    ) %>% lapply(extract_rdd_results),
    .id = "Covariates"
  ) %>% mutate(Subgroup = "CDU Support = 0")
)

# ===================================
# ISSUE OWNERSHIP EXPECTATIONS
# ===================================

data_own_1 <- data_cosine %>% filter(issue_own == 1)
data_own_0 <- data_cosine %>% filter(issue_own == 0)

models_ownership <- bind_rows(
  bind_rows(
    list(
      "No Cov" = rdrobust(data_own_1$cosine_similarity, data_own_1$distance_days, c = 0, h = bw),
      "With Cov" = rdrobust(data_own_1$cosine_similarity, data_own_1$distance_days, c = 0, h = bw,
                            covs = as.matrix(data_own_1 %>% select(all_of(control_vars))))
    ) %>% lapply(extract_rdd_results),
    .id = "Covariates"
  ) %>% mutate(Subgroup = "Issue Own = 1"),
  
  bind_rows(
    list(
      "No Cov" = rdrobust(data_own_0$cosine_similarity, data_own_0$distance_days, c = 0, h = bw),
      "With Cov" = rdrobust(data_own_0$cosine_similarity, data_own_0$distance_days, c = 0, h = bw,
                            covs = as.matrix(data_own_0 %>% select(all_of(control_vars))))
    ) %>% lapply(extract_rdd_results),
    .id = "Covariates"
  ) %>% mutate(Subgroup = "Issue Own = 0")
)

# ===================================
# Save All Model Outputs
# ===================================

if (!dir.exists("estimates")) dir.create("estimates")

write_rds(results_centrist, "estimates/rdd_cosine_centrist.rds")
write_rds(models_ownership, "estimates/rdd_cosine_issue_ownership.rds")
write_rds(models_cdu, "estimates/rdd_cosine_party_supporter.rds")
