library(testthat)

# Test 1: Basic test with simple data
test_that("trapezoid_auc works with basic data", {
  x_values <- c(0, 1, 2, 3, 4)
  y_values <- c(1, 0.8, 0.6, 0.4, 0.2)
  
  auc_result <- trapezoid_auc(x_values, y_values)
  
  # Expected result is calculated manually
  expected_auc <- 0.5 * (1 * (1 + 0.8) + 1 * (0.8 + 0.6) + 1 * (0.6 + 0.4) + 1 * (0.4 + 0.2))
  
  expect_equal(auc_result, expected_auc)
})

# Test 2: Case with single point input, should return 0 (no area)
test_that("trapezoid_auc returns 0 with a single point", {
  x_values <- c(0)
  y_values <- c(1)
  
  auc_result <- trapezoid_auc(x_values, y_values)
  
  expect_equal(auc_result, 0)
})

# Test 3: Case with empty vectors, should return an error
test_that("trapezoid_auc handles empty input", {
  expect_error(trapezoid_auc(numeric(0), numeric(0)),
               "Input vectors 'x' and 'y' must have at least two elements.")
})

# Test 4: Case with negative values for y, should still calculate AUC (possibly negative area)
test_that("trapezoid_auc works with negative y values", {
  x_values <- c(0, 1, 2, 3)
  y_values <- c(-1, -0.8, -0.6, -0.4)
  
  auc_result <- trapezoid_auc(x_values, y_values)
  
  # Manually calculated AUC for this case
  expected_auc <- 0.5 * (1 * (-1 + -0.8) + 1 * (-0.8 + -0.6) + 1 * (-0.6 + -0.4))
  
  expect_equal(auc_result, expected_auc)
})

# Test 5: Case with equal x and y values (result should be 0)
test_that("trapezoid_auc returns 0 when y values are constant", {
  x_values <- c(0, 1, 2, 3, 4)
  y_values <- c(1, 1, 1, 1, 1)
  
  auc_result <- trapezoid_auc(x_values, y_values)
  
  expect_equal(auc_result, 0)
})
