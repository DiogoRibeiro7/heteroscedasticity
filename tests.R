if (!requireNamespace("assertthat", quietly = TRUE)) install.packages("assertthat")

library(assertthat)

performWhiteTest <- function(model, data) {
  # Ensure input types are correct
  assertthat::assert_that(is.list(model), is.data.frame(data))
  
  # Documentation:
  # This function performs the White test for heteroscedasticity on a linear model.
  #
  # Args:
  #   model: An object of class 'lm', representing a fitted linear model.
  #   data: A data frame used to fit the 'lm' model. It should include all variables used in the model.
  #
  # Returns:
  #   A list containing the test statistic, its p-value, and degrees of freedom.
  
  # Extract the model's formula to identify the dependent variable
  model_formula <- formula(model)
  dependent_var <- all.vars(model_formula)[1]
  
  # Calculate squared residuals
  squared_residuals <- residuals(model)^2
  
  # Construct the model matrix for the auxiliary regression
  # Exclude the dependent variable from the model matrix
  independent_vars <- model.matrix(model_formula, data = data)
  independent_var_names <- colnames(independent_vars)[-1]  # Exclude intercept
  
  # Create auxiliary data excluding the dependent variable
  aux_data <- data.frame(independent_vars[, -1, drop = FALSE])
  names(aux_data) <- independent_var_names
  
  # Add squared terms and possibly interactions for auxiliary regression
  for (var in independent_var_names) {
    aux_data[[paste0(var, "_squared")]] <- aux_data[[var]]^2
  }
  
  # Fit the auxiliary regression model using squared residuals as the dependent variable
  aux_model <- lm(squared_residuals ~ ., data = aux_data)
  
  # Calculate the test statistic and p-value
  n <- nrow(data)
  test_statistic <- summary(aux_model)$r.squared * n
  df <- length(aux_model$coefficients) - 1  # Degrees of freedom
  p_value <- 1 - pchisq(test_statistic, df)
  
  # Return the test results
  list(test_statistic = test_statistic, p_value = p_value, degrees_of_freedom = df)
}


# Example usage (ensure you have a fitted lm model and data frame before running):
# results <- performWhiteTest(lm_model, your_data)
# print(results)


# Load necessary libraries
library(ggplot2) # For example data
library(lmtest) # For comparison with bptest

# Example data with likely homoscedasticity
data(mtcars)
model_homoscedastic <- lm(mpg ~ wt + qsec, data = mtcars)

# Artificially create heteroscedastic data
set.seed(123) # For reproducibility
mtcars$mpg_hetero <- mtcars$mpg * runif(nrow(mtcars), min = 0.5, max = 2)
model_heteroscedastic <- lm(mpg_hetero ~ wt + qsec, data = mtcars)

# Test on homoscedastic data
test_result_homoscedastic <- performWhiteTest(model_homoscedastic, mtcars)
stopifnot(is.list(test_result_homoscedastic))
stopifnot(all(names(test_result_homoscedastic) %in% c("test_statistic", "p_value", "degrees_of_freedom")))

# Test on heteroscedastic data
test_result_heteroscedastic <- performWhiteTest(model_heteroscedastic, mtcars)
stopifnot(is.list(test_result_heteroscedastic))
stopifnot(all(names(test_result_heteroscedastic) %in% c("test_statistic", "p_value", "degrees_of_freedom")))


# Assuming alpha level of 0.05 for significance
alpha <- 0.05

# Check if the function correctly fails to reject homoscedasticity
stopifnot(test_result_homoscedastic$p_value > alpha)

# Check if the function correctly identifies heteroscedasticity
stopifnot(test_result_heteroscedastic$p_value < alpha)


# Breusch-Pagan test as a comparison
bp_test_homoscedastic <- bptest(model_homoscedastic)
bp_test_heteroscedastic <- bptest(model_heteroscedastic)

# Compare p-values (roughly, since the tests are not identical)
stopifnot(abs(test_result_homoscedastic$p_value - bp_test_homoscedastic$p.value) < 0.1)
stopifnot(abs(test_result_heteroscedastic$p_value - bp_test_heteroscedastic$p.value) < 0.1)

# Should throw an error if inputs are incorrect
tryCatch({
  performWhiteTest(data = mtcars, model = "not a model")
  stop("Test failed: The function should have thrown an error with incorrect inputs.")
}, error = function(e) {
  print("Passed error handling test with incorrect model input.")
})

tryCatch({
  performWhiteTest(model = model_homoscedastic, data = "not a data frame")
  stop("Test failed: The function should have thrown an error with incorrect data input.")
}, error = function(e) {
  print("Passed error handling test with incorrect data input.")
})
