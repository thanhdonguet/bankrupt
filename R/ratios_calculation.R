# Load necessary libraries
library(dplyr)

# Load imputed analysis and validation samples
analysis_sample <- readRDS("data/analysis_sample_imputed.rds")
validation_sample <- readRDS("data/validation_sample_imputed.rds")

# Function to calculate the average for indicators over 5 years
calculate_averages <- function(data) {
  data %>%
    mutate(TotalAssets_Avg = rowMeans(select(., starts_with("TotalAssets_")), na.rm = TRUE),
           TotalLiabilities_Avg = rowMeans(select(., starts_with("TotalLiabilities_")), na.rm = TRUE),
           TotalEquity_Avg = rowMeans(select(., starts_with("TotalEquity_")), na.rm = TRUE),
           NetCFO_Avg = rowMeans(select(., starts_with("NetCFO_")), na.rm = TRUE),
           Purchases_Avg = rowMeans(select(., starts_with("Purchases_")), na.rm = TRUE))
}

# Calculate averages for both analysis and validation samples
analysis_sample <- calculate_averages(analysis_sample)
validation_sample <- calculate_averages(validation_sample)

# Now calculate ratios between the average values (example ratios)
calculate_ratios <- function(data) {
  data %>%
    mutate(Liabilities_Assets_Ratio = TotalLiabilities_Avg / TotalAssets_Avg,
           Equity_Assets_Ratio = TotalEquity_Avg / TotalAssets_Avg,
           CFO_Liabilities_Ratio = NetCFO_Avg / TotalLiabilities_Avg,
           Purchases_Assets_Ratio = Purchases_Avg / TotalAssets_Avg) %>%
    # Retain only ratios and Default_status, drop old indicators
    select(Default_status, Liabilities_Assets_Ratio, Equity_Assets_Ratio, CFO_Liabilities_Ratio, Purchases_Assets_Ratio)
}

# Calculate ratios for both samples
analysis_sample_ratios <- calculate_ratios(analysis_sample)
validation_sample_ratios <- calculate_ratios(validation_sample)

# Save the final datasets with ratios
saveRDS(analysis_sample_ratios, "data/analysis_sample_ratios.rds")
saveRDS(validation_sample_ratios, "data/validation_sample_ratios.rds")

# Print a summary to check the results
summary(analysis_sample_ratios)
summary(validation_sample_ratios)

