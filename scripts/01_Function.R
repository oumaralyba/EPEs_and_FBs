# ------------------------------------------------------------------------------
# Title: Functions for Data Analysis in R
# Context:
# This file contains a collection of functions used throughout the project. 
# These functions facilitate common tasks and ensure consistent methodology across analyses.
# ------------------------------------------------------------------------------


# Function for Truncate/Cap number to three decimal places
truncate_to_three_decimals <- function(x) {floor(x * 1000) / 1000}


# Function for calculating weighted sample means
calculate_weighted_mean <- function(model, data, weight_var) {
  mean_value <- data %>%
    filter(row_number() %in% obs(model)) %>%
    summarize(weighted_mean = weighted.mean(.data[[all.vars(formula(model))[1]]]*1000, .data[[weight_var]])) %>% 
    pull(weighted_mean)
  return(truncate_to_three_decimals(mean_value)) # Using the truncation function defined previously
}


# Function to extract and store coefficient's name, estimate, standard errors, and confidence intervals from a model
get_model_data <- function(model) {
  coef <- coef(model)
  se <- sqrt(diag(vcov(model)))
  conf_int <- confint(model)
  
  data.frame(
    term = names(coef),
    estimate = coef,
    std.error = se,
    conf.low = conf_int[, "2.5 %"],
    conf.high = conf_int[, "97.5 %"]
  )
}


