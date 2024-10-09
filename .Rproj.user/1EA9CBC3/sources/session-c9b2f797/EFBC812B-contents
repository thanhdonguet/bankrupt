# Load necessary libraries
library(dplyr)
library(car)  # For Levene's test
library(stats)  # For KS and Shapiro-Wilk tests

# Load the final cleaned validation sample
validation_sample <- readRDS("data/validation_sample_ratios.rds")

# Split validation sample into two groups based on Default_status
default_0 <- validation_sample %>% filter(Default_status == 0)
default_1 <- validation_sample %>% filter(Default_status == 1)

# Function to perform KS test and Shapiro-Wilk test
perform_normality_tests <- function(data_0, data_1, indicator) {
  ks_test_result <- ks.test(data_0[[indicator]], data_1[[indicator]])
  shapiro_0 <- shapiro.test(data_0[[indicator]])
  shapiro_1 <- shapiro.test(data_1[[indicator]])
  
  return(list(KS_Test = ks_test_result, 
              Shapiro_Test_0 = shapiro_0, 
              Shapiro_Test_1 = shapiro_1))
}

# Function to perform Levene's test for homogeneity of variance
perform_levenes_test <- function(data, indicator) {
  formula <- as.formula(paste(indicator, "~ Default_status"))
  levene_test_result <- leveneTest(formula, data = data)
  return(levene_test_result)
}

# Perform tests on all numeric indicators
indicators <- colnames(validation_sample)[sapply(validation_sample, is.numeric) & colnames(validation_sample) != "Default_status"]

# Store results
normality_results <- list()
levene_results <- list()

for (indicator in indicators) {
  # Perform normality tests (K-S and Shapiro-Wilk)
  normality_results[[indicator]] <- perform_normality_tests(default_0, default_1, indicator)
  
  # Perform Levene's test
  levene_results[[indicator]] <- perform_levenes_test(validation_sample, indicator)
}

# Save the test results to files
saveRDS(normality_results, "results/normality_tests_results.rds")
saveRDS(levene_results, "results/levene_tests_results.rds")

# Print summaries of test results
print("Kolmogorov-Smirnov and Shapiro-Wilk tests:")
print(normality_results)

print("Levene's test results:")
print(levene_results)
