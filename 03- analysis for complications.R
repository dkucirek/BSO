

# 03_complication_analysis.R

# Load necessary libraries and functions
library(tidyverse)
source("02-analysis funct.R")

# Load the cleaned data
final_data <- read_csv("cleaned_hyst_data.csv")

# --- Analysis of Any Intraoperative Complication ---

# Define predictor variables
categorical_predictors <- c("gender_identity_cat", "insurance_status_cat", "race_ethnicity_cat")
continuous_predictors <- c("age", "bmi")

# --- Fisher's Exact Tests for Categorical Predictors ---
complication_fisher_results <- list()

for (predictor in categorical_predictors) {
  result <- run_fisher_test(final_data, "any_intraop_complication", predictor)
  complication_fisher_results[[predictor]] <- result
}

# --- T-tests and Wilcoxon Tests for Continuous Predictors ---
complication_t_test_results <- list()
complication_wilcox_results <- list()

for (predictor in continuous_predictors) {
  t_test_result <- run_t_test(final_data, predictor, "any_intraop_complication")
  complication_t_test_results[[predictor]] <- t_test_result
  
  wilcox_result <- run_wilcox_test(final_data, predictor, "any_intraop_complication")
  complication_wilcox_results[[predictor]] <- wilcox_result
}

# --- Print Results ---

cat("--- Fisher's Exact Test Results for Any Intraoperative Complication ---\n")
print(complication_fisher_results)

cat("\n--- T-test Results for Any Intraoperative Complication ---\n")
print(complication_t_test_results)

cat("\n--- Wilcoxon Test Results for Any Intraoperative Complication ---\n")
print(complication_wilcox_results)
