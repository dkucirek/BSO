
# 02_analysis_functions.R

# --- Reusable Analysis Functions ---

# Function for Fisher's Exact Test
# This function takes a dataframe, an outcome variable, and a predictor variable
# and performs a Fisher's exact test.
run_fisher_test <- function(data, outcome, predictor) {
  # Create a contingency table
  contingency_table <- table(data[[outcome]], data[[predictor]])
  
  # Perform the Fisher's exact test
  fisher_result <- fisher.test(contingency_table)
  
  # Return the result
  return(fisher_result)
}

# Function for T-test
# This function takes a dataframe, a continuous variable, and a grouping variable
# and performs a t-test.
run_t_test <- function(data, continuous_var, group_var) {
  # Perform the t-test
  t_test_result <- t.test(data[[continuous_var]] ~ data[[group_var]], data = data)
  
  # Return the result
  return(t_test_result)
}

# Function for Wilcoxon Rank-Sum Test
# This function takes a dataframe, a continuous variable, and a grouping variable
# and performs a Wilcoxon rank-sum test.
run_wilcox_test <- function(data, continuous_var, group_var) {
  # Perform the Wilcoxon test
  wilcox_result <- wilcox.test(data[[continuous_var]] ~ data[[group_var]], data = data)
  
  # Return the result
  return(wilcox_result)
}

# Function to create a summary table of results
create_summary_table <- function(results_list) {
  # Create a dataframe from the list of results
  summary_df <- do.call(rbind, lapply(results_list, function(x) {
    data.frame(
      test = x$method,
      p_value = x$p.value
    )
  }))
  
  return(summary_df)
}
