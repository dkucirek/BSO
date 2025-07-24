

# 07_generate_significant_results.R

# Load libraries
library(tidyverse)

# Source analysis scripts to run them
source("03- analysis for complications.R")
source("04- ooph analysis.R")
source("05-- readmit analysis.R")

# --- Consolidate Results ---

results_list <- list(
  Complication_Fisher = complication_fisher_results,
  Complication_T_Test = complication_t_test_results,
  Complication_Wilcox = complication_wilcox_results,
  Oophorectomy_Fisher = oophorectomy_fisher_results,
  Oophorectomy_T_Test = oophorectomy_t_test_results,
  Oophorectomy_Wilcox = oophorectomy_wilcox_results,
  Readmission_Fisher = readmission_fisher_results,
  Readmission_T_Test = readmission_t_test_results,
  Readmission_Wilcox = readmission_wilcox_results
)

# --- Create a Tidy Data Frame of Results ---

tidy_results <- tibble(
  analysis = names(results_list)
) %>%
  mutate(results = results_list) %>%
  unnest_longer(results) %>%
  mutate(
    test_name = names(results),
    p_value = map_dbl(results, "p.value"),
    method = map_chr(results, "method"),
    outcome = str_extract(analysis, "^[A-Za-z]+"),
    predictor = case_when(
      test_name == "gender_identity_cat" ~ "Gender Identity",
      test_name == "insurance_status_cat" ~ "Insurance Status",
      test_name == "race_ethnicity_cat" ~ "Race/Ethnicity",
      test_name == "preop_testosterone_cat" ~ "Pre-op Testosterone Use",
      test_name == "postop_testosterone_cat" ~ "Post-op Testosterone Use",
      test_name == "preop_estrogen_cat" ~ "Pre-op Estrogen Use",
      test_name == "postop_estrogen_cat" ~ "Post-op Estrogen Use",
      test_name == "age" ~ "Age",
      test_name == "bmi" ~ "BMI",
      TRUE ~ test_name
    ),
    Analysis = case_when(
      outcome == "Complication" ~ paste("Rate of Intraoperative Complication by", predictor),
      outcome == "Oophorectomy" ~ paste("Likelihood of Oophorectomy by", predictor),
      outcome == "Readmission" ~ paste("Rate of Post-operative Readmission by", predictor),
      TRUE ~ "Analysis"
    ),
    p_value_formatted = case_when(
      p_value < 0.001 ~ sprintf("%.3e", p_value),
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) %>%
  select(Analysis, p_value, p_value_formatted, method)

# --- Filter for Significant Results ---

significant_results <- tidy_results %>%
  filter(p_value <= 0.05) %>%
  select(Analysis, p_value_formatted, method)

# --- Save Significant Results ---

write_csv(significant_results, "significant_results.csv")

cat("Significant results saved to significant_results.csv\n")

