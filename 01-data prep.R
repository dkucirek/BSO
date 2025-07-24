
# 01_data_preparation.R

# Load necessary libraries
library(tidyverse)
library(readxl)
library(janitor)
library(conflicted)
# --- Load Data ---

# It appears there are two primary data sources. Based on the file names,
# I will assume "COMBINED HYST DATA.xlsx" is the most complete and recent one.
# If this is incorrect, please specify which file to use.
raw_data <- read_excel("COMBINED HYST DATA.xlsx")

# --- Clean and Prepare Data ---

# Rename columns to be more R-friendly
cleaned_data <- raw_data %>%
  clean_names()

# Select and rename relevant columns for clarity
# This is based on the variables used across the various analysis scripts.
cleaned_data <- cleaned_data %>%
  select(
    age,
    bmi,
    current_gender_identity,
    insurance_status,
    race_ethnicity,
    preop_testosterone,
    postop_testosterone,
    oophorectomy,
    preop_estrogen,
    postop_estrogen,
    intraop_complications_1, # No
    intraop_complications_2, # Hemorrhage
    intraop_complications_3, # Bowel injury
    intraop_complications_4, # Bladder injury
    intraop_complications_5, # Ureter injury
    intraop_complications_6, # Other
    intraop_comp_other,
    postop_complications_2 # Readmission
  )

# --- Recode Variables ---

# Oophorectomy (Bilateral vs. No/Unilateral)
cleaned_data <- cleaned_data %>%
  mutate(
    oophorectomy_binary = case_when(
      oophorectomy == 3 ~ "Yes",  # Bilateral
      oophorectomy %in% c(1, 2) ~ "No",   # No or Unilateral
      TRUE ~ NA_character_
    )
  )

# Race/Ethnicity
cleaned_data <- cleaned_data %>%
  mutate(race_ethnicity_cat = case_when(
    race_ethnicity == 1 ~ "American Indian or Alaska Native",
    race_ethnicity == 2 ~ "Asian",
    race_ethnicity == 3 ~ "Black or African American",
    race_ethnicity == 4 ~ "Native Hawaiian or Other Pacific Islander",
    race_ethnicity == 5 ~ "White",
    race_ethnicity == 6 ~ "Hispanic/Latinx",
    race_ethnicity == 7 ~ "Multiracial/Multiethnic",
    race_ethnicity == 8 ~ "Other",
    race_ethnicity == 9 ~ "Unknown or not reported",
    TRUE ~ NA_character_
  ))

# Insurance Status
cleaned_data <- cleaned_data %>%
  mutate(insurance_status_cat = case_when(
    insurance_status == 1 ~ "Public",
    insurance_status == 2 ~ "Private",
    insurance_status == 3 ~ "Uninsured",
    insurance_status == 5 ~ "Self Pay",
    insurance_status == 6 ~ "Other",
    TRUE ~ NA_character_
  ))

# Gender Identity
cleaned_data <- cleaned_data %>%
  mutate(gender_identity_cat = case_when(
    current_gender_identity == 4 ~ "Trans Male",
    current_gender_identity == 5 ~ "Nonbinary",
    TRUE ~ NA_character_
  ))

# Pre-op Testosterone
cleaned_data <- cleaned_data %>%
  mutate(preop_testosterone_cat = case_when(
    preop_testosterone == 1 ~ "Yes",
    preop_testosterone == 2 ~ "No",
    TRUE ~ NA_character_
  ))

# Post-op Testosterone
cleaned_data <- cleaned_data %>%
  mutate(postop_testosterone_cat = case_when(
    postop_testosterone == 1 ~ "Yes",
    postop_testosterone == 2 ~ "No",
    TRUE ~ NA_character_
  ))

# Pre-op Estrogen
cleaned_data <- cleaned_data %>%
  mutate(preop_estrogen_cat = case_when(
    preop_estrogen == 1 ~ "Yes",
    preop_estrogen == 2 ~ "No",
    TRUE ~ NA_character_
  ))

# Post-op Estrogen
cleaned_data <- cleaned_data %>%
  mutate(postop_estrogen_cat = case_when(
    postop_estrogen == 1 ~ "Yes",
    postop_estrogen == 2 ~ "No",
    TRUE ~ NA_character_
  ))

# --- Create Composite Variables ---

# Any Intraoperative Complication
cleaned_data <- cleaned_data %>%
  mutate(
    any_intraop_complication = if_else(
      rowSums(across(c(
        intraop_complications_2, intraop_complications_3,
        intraop_complications_4, intraop_complications_5,
        intraop_complications_6
      )), na.rm = TRUE) > 0 | !is.na(intraop_comp_other),
      "Yes", "No"
    )
  )

# Readmission
cleaned_data <- cleaned_data %>%
  mutate(
    readmission = if_else(postop_complications_2 == 1, "Yes", "No")
  )

# --- Finalize Data ---

# Select final set of variables
final_data <- cleaned_data %>%
  select(
    age,
    bmi,
    gender_identity_cat,
    insurance_status_cat,
    race_ethnicity_cat,
    preop_testosterone_cat,
    postop_testosterone_cat,
    preop_estrogen_cat,
    postop_estrogen_cat,
    oophorectomy_binary,
    any_intraop_complication,
    readmission,
    hemorrhage = intraop_complications_2,
    bowel_injury = intraop_complications_3,
    bladder_injury = intraop_complications_4,
    ureter_injury = intraop_complications_5,
    other_complication = intraop_complications_6
  )

# Save the cleaned data
write_csv(final_data, "cleaned_hyst_data.csv")

# Print a summary of the final data
glimpse(final_data)