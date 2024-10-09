# Load necessary libraries
library(dplyr)
library(caret)  # For creating data partitions

# Load cleaned data
clean_data <- readRDS("data/clean_data.rds")

# Ensure that the Default_status column is properly factored (important for caret)
clean_data$Default_status <- as.factor(clean_data$Default_status)

# Split the data into 50% analysis sample and 50% validation sample
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(clean_data$Default_status, p = 0.5, list = FALSE)

# Analysis sample (50%)
analysis_sample <- clean_data[trainIndex, ]

# Validation sample (50%)
validation_sample <- clean_data[-trainIndex, ]

# Save both samples into separate RDS files
saveRDS(analysis_sample, "data/analysis_sample.rds")
saveRDS(validation_sample, "data/validation_sample.rds")

# Print summaries to check the splits
summary(analysis_sample)
summary(validation_sample)
