# Thesis Scherer – Bandwidth Sensitivity Analysis (Cosine Similarity)
# ===================================================================

# Load libraries
library(tidyverse)
library(lubridate)
library(rdrobust)
library(gridExtra)
library(showtext)

# Font setup for academic figures
font_add("CMU Serif", "C:/Users/phili/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf")
showtext_auto()
theme_set(theme_minimal(base_family = "CMU Serif"))

# Load cleaned dataset
data_cosine <- read_rds("V1data_cosine.rds") %>%
  filter(distance_days >= -40 & distance_days <= 40) %>%
  filter(category_opinion == category_speech) %>%
  mutate(
    weekday = as.numeric(weekday),
    month = as.numeric(month)
  )

# Define covariates
control_vars <- c("weekday", "month", "year", "election_year", "legislative_period")
control_vars <- control_vars[control_vars %in% colnames(data_cosine)]

# Bandwidths to test
bandwidths <- seq(4, 40, by = 4)

# Containers for estimates and CIs
rdd_plot_df_nocov <- tibble(bandwidth = bandwidths, estimate = NA, ci_lower = NA, ci_upper = NA)
rdd_plot_df_cov   <- rdd_plot_df_nocov

# Loop over bandwidths
for (i in seq_along(bandwidths)) {
  
  bw <- bandwidths[i]
  
  # No covariates
  model_nocov <- rdrobust(
    y = data_cosine$cosine_similarity,
    x = data_cosine$distance_days,
    c = 0,
    h = bw
  )
  
  # With covariates
  model_cov <- rdrobust(
    y = data_cosine$cosine_similarity,
    x = data_cosine$distance_days,
    c = 0,
    h = bw,
    covs = as.matrix(data_cosine %>% select(all_of(control_vars)))
  )
  
  # Save for plotting
  rdd_plot_df_nocov[i, 2:4] <- c(model_nocov$coef[1], model_nocov$ci[1, ])
  rdd_plot_df_cov[i, 2:4]   <- c(model_cov$coef[1], model_cov$ci[1, ])
}

# Set y-axis limits
y_min <- min(rdd_plot_df_nocov$ci_lower, rdd_plot_df_cov$ci_lower)
y_max <- max(rdd_plot_df_nocov$ci_upper, rdd_plot_df_cov$ci_upper)

# Plot: No covariates
p_nocov <- ggplot(rdd_plot_df_nocov, aes(x = bandwidth, y = estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, color = "black") +
  labs(title = "No Covariates", x = "Bandwidth (days)", y = "") +
  ylim(y_min, y_max)

# Plot: With covariates
p_cov <- ggplot(rdd_plot_df_cov, aes(x = bandwidth, y = estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, color = "black") +
  labs(title = "Covariates Included", x = "Bandwidth (days)", y = "") +
  ylim(y_min, y_max)

# Show both plots side by side
grid.arrange(p_nocov, p_cov, ncol = 2)


# -------------------------------------- #
# Full estimates (table format)
# -------------------------------------- #

model_list_nocov <- list()
model_list_cov   <- list()
rdd_results_df   <- tibble()

extract_rdrobust <- function(model, bw, cov_label) {
  tibble(
    Bandwidth = bw,
    Covariates = cov_label,
    Coef = model$coef["Conventional", "Coeff"],
    SE = model$se["Conventional", "Std. Err."],
    P_Value = model$pv["Conventional", "P>|z|"],
    Robust_Coef = model$coef["Robust", "Coeff"],
    Robust_SE = model$se["Robust", "Std. Err."],
    Robust_P_Value = model$pv["Robust", "P>|z|"],
    N = sum(model$N)
  )
}

# Loop through bandwidths
for (i in seq_along(bandwidths)) {
  
  bw <- bandwidths[i]
  
  mod_nocov <- rdrobust(
    y = data_cosine$cosine_similarity,
    x = data_cosine$distance_days,
    c = 0,
    h = bw
  )
  
  mod_cov <- rdrobust(
    y = data_cosine$cosine_similarity,
    x = data_cosine$distance_days,
    c = 0,
    h = bw,
    covs = as.matrix(data_cosine %>% select(all_of(control_vars)))
  )
  
  # Save models (if needed later)
  model_list_nocov[[i]] <- mod_nocov
  model_list_cov[[i]]   <- mod_cov
  
  # Append results
  rdd_results_df <- bind_rows(
    rdd_results_df,
    extract_rdrobust(mod_nocov, bw, "No"),
    extract_rdrobust(mod_cov, bw, "Yes")
  )
}

# Save results
if (!dir.exists("estimates")) dir.create("estimates")
write_rds(rdd_results_df, "estimates/estimates_bandwidth_variation.rds")

