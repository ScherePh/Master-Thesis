
#### Cosine Similarity

library(tidyverse)
library(lubridate)
library(rdrobust)
library(gridExtra)
library(showtext)

# Load CMU Serif font
font_add("CMU Serif", "C:/Users/phili/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf")  
showtext_auto()  # Enable showtext for ggplot rendering

# Load preprocessed data
merged_data <- read_rds("V1data_cosine.rds")

# Filter for the ±30-day window
rdd_data <- merged_data %>%
  filter(distance_days >= -50 & distance_days <= 50) %>%
  filter(category_opinion == category_speech)

# Convert categorical variables to numeric for rdrobust
rdd_data <- rdd_data %>%
  mutate(
    weekday = as.numeric(weekday),
    month = as.numeric(month)
  )

# Define control variables
control_vars <- c("weekday", "month", "year", "election_year", "legislative_period")
control_vars <- control_vars[control_vars %in% colnames(rdd_data)]


####

library(parallel)

# Define range of cutoff values (-30 to +30)
cutoff_values <- seq(-30, 30, by = 5)

# Initialize tibble for storing results
rdd_results_with_covariates <- tibble(cutoff = cutoff_values,
                                      estimate = NA, ci_lower = NA, ci_upper = NA)

# Function for running RDD with covariates at a given cutoff
run_rdd_with_covariates <- function(cutoff) {
  model <- rdrobust(y = rdd_data$cosine_similarity, 
                    x = rdd_data$distance_days, 
                    c = cutoff, 
                    h = optimal_bandwidth, 
                    covs = as.matrix(rdd_data %>% select(all_of(control_vars))))
  
  return(c(estimate = model$coef[1], 
           ci_lower = model$ci[1,1], 
           ci_upper = model$ci[1,2]))
}

# Set up parallel processing (Windows-compatible)
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)  # Create cluster

# Export required objects to worker nodes
clusterExport(cl, varlist = c("rdd_data", "optimal_bandwidth", "control_vars", "run_rdd_with_covariates"))
clusterEvalQ(cl, {
  library(rdrobust)
  library(dplyr)  # ✅ Ensure dplyr is loaded
  library(lubridate)
})

# Run RDD models in parallel
results <- parLapply(cl, cutoff_values, run_rdd_with_covariates)

# Stop the cluster
stopCluster(cl)

# Convert results to a dataframe
rdd_results_with_covariates[, c("estimate", "ci_lower", "ci_upper")] <- do.call(rbind, results)


rdd_results_with_covariates <- rdd_results_with_covariates %>% 
  filter(cutoff >= -25 & cutoff <= 25)
  
# Define y-axis limits for consistency
y_min <- min(rdd_results_with_covariates$ci_lower, na.rm = TRUE)
y_max <- max(rdd_results_with_covariates$ci_upper, na.rm = TRUE)

# RDD with covariates
p_with_cov <- ggplot(rdd_results_with_covariates, aes(x = cutoff, y = estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, color = "black") +
  labs(title = "", 
       x = "Cutoff Dates", 
       y = "") +
  ylim(y_min, y_max) +
  theme_minimal(base_family = "CMU Serif")

# Display plot
print(p_with_cov)





#####

#### Cosine Similarity

library(tidyverse)
library(lubridate)
library(rdrobust)
library(gridExtra)
library(showtext)

# Load CMU Serif font
font_add("CMU Serif", "C:/Users/phili/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf")  
showtext_auto()  # Enable showtext for ggplot rendering

# Load preprocessed data
merged_data <- read_rds("V1data_cosine.rds")

# Filter for the ±30-day window
rdd_data <- merged_data %>%
  filter(distance_days >= -50 & distance_days <= 50) %>%
  filter(category_opinion == category_speech)

# Convert categorical variables to numeric for rdrobust
rdd_data <- rdd_data %>%
  mutate(
    weekday = as.numeric(weekday),
    month = as.numeric(month)
  )

# Define control variables
control_vars <- c("weekday", "month", "year", "election_year", "legislative_period")
control_vars <- control_vars[control_vars %in% colnames(rdd_data)]


####

library(parallel)

optimal_bandwidth <- 5

# Define range of cutoff values (-30 to +30)
cutoff_values <- seq(-15, 15, by = 1)

# Initialize tibble for storing results
rdd_results_with_covariates <- tibble(cutoff = cutoff_values,
                                      estimate = NA, ci_lower = NA, ci_upper = NA)

# Function for running RDD with covariates at a given cutoff
run_rdd_with_covariates <- function(cutoff) {
  model <- rdrobust(y = rdd_data$cosine_similarity, 
                    x = rdd_data$distance_days, 
                    c = cutoff, 
                    h = optimal_bandwidth, 
                    covs = as.matrix(rdd_data %>% select(all_of(control_vars))))
  
  return(c(estimate = model$coef[1], 
           ci_lower = model$ci[1,1], 
           ci_upper = model$ci[1,2]))
}

# Set up parallel processing (Windows-compatible)
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)  # Create cluster

# Export required objects to worker nodes
clusterExport(cl, varlist = c("rdd_data", "optimal_bandwidth", "control_vars", "run_rdd_with_covariates"))
clusterEvalQ(cl, {
  library(rdrobust)
  library(dplyr)  # ✅ Ensure dplyr is loaded
  library(lubridate)
})

# Run RDD models in parallel
results <- parLapply(cl, cutoff_values, run_rdd_with_covariates)

# Stop the cluster
stopCluster(cl)

# Convert results to a dataframe
rdd_results_with_covariates[, c("estimate", "ci_lower", "ci_upper")] <- do.call(rbind, results)


rdd_results_with_covariates <- rdd_results_with_covariates %>% 
  filter(cutoff >= -8 & cutoff <= 8)

# Define y-axis limits for consistency
y_min <- min(rdd_results_with_covariates$ci_lower, na.rm = TRUE)
y_max <- max(rdd_results_with_covariates$ci_upper, na.rm = TRUE)

# RDD with covariates
p_with_cov <- ggplot(rdd_results_with_covariates, aes(x = cutoff, y = estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, color = "black") +
  labs(title = "", 
       x = "Cutoff Dates", 
       y = "") +
  ylim(y_min, y_max) +
  theme_minimal(base_family = "CMU Serif")

# Display plot
print(p_with_cov)

