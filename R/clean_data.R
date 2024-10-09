# Load necessary libraries
library(dplyr)

# Load raw data
raw_data <- readRDS("data/raw_data.rds")

# Define important indicators: Total Assets, Total Liabilities, Total Equity
important_indicators <- c("TotalAssets_15", "TotalAssets_16", "TotalAssets_17", "TotalAssets_18", "TotalAssets_19", 
                          "TotalLiabilities_15", "TotalLiabilities_16", "TotalLiabilities_17", "TotalLiabilities_18", "TotalLiabilities_19",
                          "TotalEquity_15", "TotalEquity_16", "TotalEquity_17", "TotalEquity_18", "TotalEquity_19")

# Rule 1: Remove observations with missing important indicators (any missing values in these columns)
clean_data <- raw_data %>%
  filter(complete.cases(select(., all_of(important_indicators))))

# Rule 2: Remove observations missing more than 75% of all indicators
# Calculate percentage of missing values for each row
row_missing_percentage <- rowMeans(is.na(clean_data))

# Keep only rows with less than 75% missing data
clean_data <- clean_data %>%
  filter(row_missing_percentage <= 0.75)

# Rule 3: Remove indicators (columns) that have more than 10% missing values
# Calculate percentage of missing values for each column
col_missing_percentage <- colMeans(is.na(clean_data))

# Keep only columns where less than 10% of values are missing
clean_data <- clean_data %>%
  select(which(col_missing_percentage <= 0.10))

# Rule 4: Remove duplicate rows
clean_data <- clean_data %>% distinct()

# Save cleaned data
saveRDS(clean_data, "data/clean_data.rds")

# Print summary to ensure cleaning steps were applied correctly
summary(clean_data)
