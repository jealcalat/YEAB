test_that("hyperbolic_fit works with valid inputs", {
  value <- c(1, 0.8, 0.6, 0.5, 0.3)
  delay <- c(0, 1, 2, 3, 5)
  model <- hyperbolic_fit(value, delay, initial_guess = 0.1)
  
  expect_s3_class(model, "nls")
  expect_named(coef(model), "k")
})

test_that("hyperbolic_fit handles invalid inputs", {
  value <- c(1, 0.8, "a", 0.5, 0.3) # Invalid input
  delay <- c(0, 1, 2, 3, 5)
  
  expect_error(hyperbolic_fit(value, delay, initial_guess = 0.1), 
               "Arguments should be numeric")
  
  expect_error(hyperbolic_fit(value, delay, initial_guess = "0.1"), 
               "Arguments should be numeric")
})

test_that("exp_fit works with valid inputs", {
  value <- c(1, 0.8, 0.6, 0.5, 0.3)
  delay <- c(0, 1, 2, 3, 5)
  model <- exp_fit(value, delay, initial_guess = 0.1)
  
  expect_s3_class(model, "nls")
  expect_named(coef(model), "k")
})

test_that("exp_fit handles invalid inputs", {
  value <- c(1, 0.8, 0.6, 0.5, 0.3)
  delay <- c(0, 1, 2, 3, 5)
  
  expect_error(exp_fit(value, delay, initial_guess = "abc"), 
               "Arguments should be numeric")
  
  expect_error(exp_fit(value, delay = "invalid", initial_guess = 0.1), 
               "Arguments should be numeric")
})

