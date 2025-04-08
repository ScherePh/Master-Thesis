# Thesis Scherer – Main Models Hand-Coded Sample
# ==============================================
# RDD Models based on hand-coded sample


# Load libraries
library(tidyverse)
library(lubridate)
library(rdrobust)

# Load and prepare data
coded_data <- read.csv2("manual_coding_sample_coded.csv")

coded_data <- coded_data %>%
  na.omit() %>%
  mutate(
    weekday = as.numeric(factor(weekday, levels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"))),
    month = as.numeric(factor(month, levels = month.name))
  )

# Define covariates
control_vars <- c("weekday", "month", "year", "election_year", "legislative_period")


# ===================================
# CENTRIST EXPECTATIONS
# ===================================

# Main model with and without covariates
rdd_manual_covs <- rdrobust(
  y = as.numeric(coded_data$manual_agreement),
  x = as.numeric(coded_data$distance_days),
  c = 0, h = 5,
  covs = as.matrix(coded_data %>% select(all_of(control_vars))),
  weights = coded_data$percentage
)

rdd_manual_nocovs <- rdrobust(
  y = as.numeric(coded_data$manual_agreement),
  x = as.numeric(coded_data$distance_days),
  c = 0, h = 5,
  weights = coded_data$percentage
)

# Function to extract RDD output
extract_rdd_results <- function(rdd_object) {
  data.frame(
    Coef = rdd_object$coef["Conventional", "Coeff"],
    SE = rdd_object$se["Conventional", "Std. Err."],
    P_Value = rdd_object$pv["Conventional", "P>|z|"],
    Robust_Coef = rdd_object$coef["Robust", "Coeff"],
    Robust_SE = rdd_object$se["Robust", "Std. Err."],
    Robust_P_Value = rdd_object$pv["Robust", "P>|z|"],
    N = rdd_object$N[1]
  )
}

manual_main_df <- bind_rows(
  extract_rdd_results(rdd_manual_covs) %>% mutate(Model = "With Covariates"),
  extract_rdd_results(rdd_manual_nocovs) %>% mutate(Model = "No Covariates")
) %>% select(Model, everything())

# ===================================
# PARTY SUPPORTER EXPECTATIONS
# ===================================

manual_cdu_df <- bind_rows(
  coded_data %>% filter(cdu_supporter == 1) %>%
    { rdrobust(y = .$manual_agreement, x = .$distance_days, c = 0, h = 5,
               covs = as.matrix(select(., all_of(control_vars))),
               weights = .$percentage) } %>%
    extract_rdd_results() %>%
    mutate(Subgroup = "CDU Supporter = 1"),
  
  coded_data %>% filter(cdu_supporter == 0) %>%
    { rdrobust(y = .$manual_agreement, x = .$distance_days, c = 0, h = 5,
               covs = as.matrix(select(., all_of(control_vars))),
               weights = .$percentage) } %>%
    extract_rdd_results() %>%
    mutate(Subgroup = "CDU Supporter = 0")
)

# ===================================
# ISSUE OWNERSHIP EXPECTATIONS
# ===================================

manual_issue_df <- bind_rows(
  coded_data %>% filter(issue_own == 1) %>%
    { rdrobust(y = .$manual_agreement, x = .$distance_days, c = 0, h = 5,
               covs = as.matrix(select(., all_of(control_vars))),
               weights = .$percentage) } %>%
    extract_rdd_results() %>%
    mutate(Subgroup = "Issue Ownership = 1"),
  
  coded_data %>% filter(issue_own == 0) %>%
    { rdrobust(y = .$manual_agreement, x = .$distance_days, c = 0, h = 5,
               covs = as.matrix(select(., all_of(control_vars))),
               weights = .$percentage) } %>%
    extract_rdd_results() %>%
    mutate(Subgroup = "Issue Ownership = 0")
)

# ===================================
# Save Results
# ===================================
write_rds(manual_main_df, "estimates/rdd_manual_centrist.rds")
write_rds(manual_cdu_df, "estimates/rdd_manual_party_supporter.rds")
write_rds(manual_issue_df, "estimates/rdd_manual_issue_ownership.rds")

