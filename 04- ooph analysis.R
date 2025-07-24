

# 04_oophorectomy_analysis.R

# Load necessary libraries and functions
library(tidyverse)
source("02-analysis funct.R")

# Load the cleaned data
final_data <- read_csv("cleaned_hyst_data.csv")

# --- Analysis of Oophorectomy ---

# Define predictor variables
categorical_predictors <- c(
  "gender_identity_cat", "insurance_status_cat", "race_ethnicity_cat",
  "preop_testosterone_cat", "postop_testosterone_cat",
  "preop_estrogen_cat", "postop_estrogen_cat"
)
continuous_predictors <- c("age", "bmi")

# --- Fisher's Exact Tests for Categorical Predictors ---
oophorectomy_fisher_results <- list()

for (predictor in categorical_predictors) {
  result <- run_fisher_test(final_data, "oophorectomy_binary", predictor)
  oophorectomy_fisher_results[[predictor]] <- result
}

# --- T-tests and Wilcoxon Tests for Continuous Predictors ---
oophorectomy_t_test_results <- list()
oophorectomy_wilcox_results <- list()

for (predictor in continuous_predictors) {
  t_test_result <- run_t_test(final_data, predictor, "oophorectomy_binary")
  oophorectomy_t_test_results[[predictor]] <- t_test_result
  
  wilcox_result <- run_wilcox_test(final_data, predictor, "oophorectomy_binary")
  oophorectomy_wilcox_results[[predictor]] <- wilcox_result
}

# --- Print Results ---

cat("--- Fisher's Exact Test Results for Oophorectomy ---\n")
print(oophorectomy_fisher_results)

cat("\n--- T-test Results for Oophorectomy ---\n")
print(oophorectomy_t_test_results)

cat("\n--- Wilcoxon Test Results for Oophorectomy ---\n")
print(oophorectomy_wilcox_results)
