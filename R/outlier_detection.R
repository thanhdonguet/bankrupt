# Load necessary libraries
library(dplyr)

# Load analysis sample ratios
analysis_sample_ratios <- readRDS("data/analysis_sample_ratios.rds")

# Function to detect outliers based on Interquartile Range (IQR)
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  outliers <- x < lower_bound | x > upper_bound
  return(outliers)
}

# Apply outlier detection and calculate percentage of outliers for each indicator
outlier_stats <- analysis_sample_ratios %>%
  summarise(across(where(is.numeric), 
                   ~mean(detect_outliers(.), na.rm = TRUE) * 100))

# Identify indicators with more than 10% outliers
indicators_with_outliers <- names(outlier_stats)[outlier_stats[1, ] > 10]

# Logistic transformation function
logistic_transform <- function(x) {
  # Normalize to (0, 1) before applying logistic transformation
  normalized_x <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  transformed_x <- log(normalized_x / (1 - normalized_x + 1e-6))  # Adding small value to prevent division by zero
  return(transformed_x)
}

# Apply logistic transformation to indicators with more than 10% outliers
analysis_sample_transformed <- analysis_sample_ratios %>%
  mutate(across(all_of(indicators_with_outliers), logistic_transform))

# Check outliers again after transformation
outlier_stats_after_transform <- analysis_sample_transformed %>%
  summarise(across(all_of(indicators_with_outliers), 
                   ~mean(detect_outliers(.), na.rm = TRUE) * 100))

# Identify indicators that still have more than 10% outliers after transformation
indicators_to_remove <- names(outlier_stats_after_transform)[outlier_stats_after_transform[1, ] > 10]

# Remove the problematic indicators
analysis_sample_final <- analysis_sample_transformed %>%
  select(-all_of(indicators_to_remove))

# Save the final cleaned dataset
saveRDS(analysis_sample_final, "data/analysis_sample_final.rds")

# Print summary of the final dataset
summary(analysis_sample_final)
