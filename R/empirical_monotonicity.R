# Load necessary libraries
library(dplyr)

# Load the final cleaned analysis sample
analysis_sample <- readRDS("data/analysis_sample_final.rds")

# Function to calculate empirical monotonicity
# We bin the continuous variables into 5 equal-sized bins and check monotonicity
calculate_monotonicity <- function(data, indicator, bins = 5) {
  # Cut the indicator into bins
  data <- data %>%
    mutate(binned = ntile(!!sym(indicator), bins))
  
  # Calculate the proportion of bankrupt companies in each bin
  monotonicity <- data %>%
    group_by(binned) %>%
    summarise(bankrupt_proportion = mean(Default_status == 1)) %>%
    arrange(binned)
  
  # Check if the bankrupt proportion is monotonically increasing or decreasing
  is_monotonic <- all(diff(monotonicity$bankrupt_proportion) >= 0) || 
    all(diff(monotonicity$bankrupt_proportion) <= 0)
  
  return(is_monotonic)
}

# Apply monotonicity check to all numeric indicators
indicators <- colnames(analysis_sample)[sapply(analysis_sample, is.numeric) & colnames(analysis_sample) != "Default_status"]

monotonicity_results <- sapply(indicators, function(indicator) {
  calculate_monotonicity(analysis_sample, indicator)
})

# Keep only indicators with monotonic relationships
monotonic_indicators <- indicators[monotonicity_results]

# Filter the dataset to retain only monotonic indicators and Default_status
analysis_sample_monotonic <- analysis_sample %>%
  select(Default_status, all_of(monotonic_indicators))

# Save the final dataset with monotonic indicators
saveRDS(analysis_sample_monotonic, "data/analysis_sample_monotonic.rds")

# Print the results
print(monotonic_indicators)
