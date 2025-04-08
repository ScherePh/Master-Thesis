# Thesis Scherer – Randomization Inference 
# =============================================================
# RDD with artificial placebo cutoffs using randomization inference.
# Important: extensive computation!


# Load Libraries
library(tidyverse)
library(lubridate)
library(rdrobust)
library(parallel)

# Load Data (preprocessed)
rdd_data <-  read_rds("V2_cosine_full.rds")  # replace with your object if needed

# Fix bandwidth
bw <- 5
cutoff <- 0

# Run Baseline RDD
rdd_model <- rdrobust(
  y = rdd_data$cosine_similarity,
  x = rdd_data$distance_days,
  c = cutoff,
  h = bw,
  covs = rdd_data$weekend
)

baseline_late <- rdd_model$coef[1]

# Helper Function
rdd_aux <- function(data, runvar = "distance_days", yvar = "cosine_similarity", bw = 5) {
  est <- rdrobust(y = data[[yvar]], x = data[[runvar]], c = 0, h = bw)
  return(c("coef" = est$coef[1], "p_val" = est$p[1]))
}

# Randomization Test Function
perm_test <- function(data = rdd_data,
                      runvar = "distance_days",
                      yvar = "cosine_similarity",
                      bw = 5,
                      report_date_var = "date_opinion",
                      speech_date_var = "date_speech",
                      n_rep = 125) {
  
  possible_dates <- seq(min(data[[report_date_var]], na.rm = TRUE),
                        max(data[[report_date_var]], na.rm = TRUE),
                        by = "day")
  
  # Remove weekends
  possible_dates <- possible_dates[!weekdays(possible_dates) %in% c("Saturday", "Sunday")]
  
  # Sample new dates
  new_release_dates <- sample(possible_dates, n_rep, replace = TRUE)
  
  temp_data <- data
  temp_data[[report_date_var]] <- rep(new_release_dates, length.out = nrow(temp_data))
  temp_data[[runvar]] <- as.numeric(temp_data[[speech_date_var]] - temp_data[[report_date_var]])
  
  rdd_aux(temp_data, runvar = runvar, yvar = yvar, bw = bw)
}

# Run Simulation (Full)
set.seed(1234)
n_iter <- 1000
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)

clusterEvalQ(cl, library(rdrobust))
clusterExport(cl, c("rdd_data", "rdd_aux", "perm_test", "cutoff", "bw"))

system.time({
  perm_res <- parSapply(cl, 1:n_iter, function(x) {
    perm_test(data = rdd_data,
              runvar = "distance_days",
              yvar = "cosine_similarity",
              bw = bw,
              report_date_var = "date_opinion",
              speech_date_var = "date_speech",
              n_rep = 125)
  })
})

stopCluster(cl)

# Prepare Results
perm_df <- as.data.frame(t(perm_res))
colnames(perm_df) <- c("coef", "p_val")

# Compute randomization p-value
ri_pval <- sum(abs(perm_df$coef) > abs(baseline_late)) / n_iter

# Create annotation text as data frame
annotation_df <- data.frame(
  x = min(perm_df$coef) + 0.0002,
  y = max(hist(perm_df$coef, plot = FALSE)$counts),
  label = paste("p =", round(ri_pval, 3))
)

# Final Plot
ggplot(perm_df, aes(x = coef)) +
  geom_histogram(fill = "grey80", color = "black", binwidth = 0.0002) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = baseline_late, color = "blue") +
  geom_text(data = annotation_df, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4) +
  labs(title = "Randomization Inference: Distribution of LATE",
       x = "Estimated Effect",
       y = "Frequency") +
  theme_minimal()
