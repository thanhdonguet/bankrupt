# Load necessary libraries
library(dplyr)

# Load validation sample data
validation_sample <- readRDS("data/validation_sample.rds")

# Separate validation_sample into two groups based on Default_status
default_1 <- validation_sample %>% filter(Default_status == 1)  # Bankrupt companies
default_0 <- validation_sample %>% filter(Default_status == 0)  # Non-bankrupt companies

# Descriptive statistics function
calculate_stats <- function(data) {
  stats <- data %>%
    summarise(across(where(is.numeric), 
                     list(min = min, max = max, mean = mean, median = median, sd = sd), na.rm = TRUE))
  return(stats)
}

# Calculate descriptive statistics for each group
stats_default_1 <- calculate_stats(default_1)
stats_default_0 <- calculate_stats(default_0)

# Save the descriptive statistics to CSV files
write.csv(stats_default_1, "analysis/stats_validation_default_1.csv", row.names = FALSE)
write.csv(stats_default_0, "analysis/stats_validation_default_0.csv", row.names = FALSE)

# Function to replace missing values with median
replace_na_with_median <- function(data) {
  data_imputed <- data %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  return(data_imputed)
}

# Replace missing values for Default_status 1 group
default_1_imputed <- replace_na_with_median(default_1)

# Replace missing values for Default_status 0 group
default_0_imputed <- replace_na_with_median(default_0)

# Combine the imputed groups back into one dataset
validation_sample_imputed <- bind_rows(default_1_imputed, default_0_imputed)

# Save the final imputed validation sample
saveRDS(validation_sample_imputed, "data/validation_sample_imputed.rds")

# Print summary to check the imputation
summary(validation_sample_imputed)
