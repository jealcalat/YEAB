# test-entropy.R

library(testthat)
library(MASS)

# Test 1: Ensure the function returns a numeric value
test_that("entropy_kde2d returns a numeric value", {
  set.seed(123)
  n <- 1000
  x <- rnorm(n)
  y <- rnorm(n)
  result <- entropy_kde2d(x, y, n_grid = 50)
  expect_type(result, "double")
})

# Test 2: Handle empty input
test_that("entropy_kde2d handles empty input", {
  expect_error(entropy_kde2d(numeric(0), numeric(0), n_grid = 50),
               "Input vectors 'x' and 'y' must have at least two elements.")
})

# Test 3: Check the result for a normal distribution (entropy should be non-zero)
test_that("entropy_kde2d computes non-zero entropy for normal distribution", {
  set.seed(123)
  n <- 1000
  mean <- c(0, 0)
  sd_x <- 1
  sd_y <- 1
  correlation <- 0.5
  sigma <- matrix(
    c(sd_x^2, correlation * sd_x * sd_y,
      correlation * sd_x * sd_y, sd_y^2),
    ncol = 2
  )
  simulated_data <- mvrnorm(n, mu = mean, Sigma = sigma)
  x <- simulated_data[, 1]
  y <- simulated_data[, 2]
  result <- entropy_kde2d(x, y, n_grid = 50)
  expect_type(result, "double")
  expect_gt(result, 0)
})

# Test 4: Check if the function works with a very small grid size
test_that("entropy_kde2d works with a small grid size", {
  set.seed(123)
  n <- 1000
  x <- rnorm(n)
  y <- rnorm(n)
  result <- entropy_kde2d(x, y, n_grid = 10)
  expect_type(result, "double")
})

# Test 5: Check if the function handles large datasets (performance test)
test_that("entropy_kde2d handles large datasets", {
  set.seed(123)
  n <- 10000
  x <- rnorm(n)
  y <- rnorm(n)
  expect_silent(entropy_kde2d(x, y, n_grid = 100))
})

# Test 6: Check for error when n_grid is too small (less than 1)
test_that("entropy_kde2d throws an error when n_grid is too small", {
  set.seed(123)
  x <- rnorm(100)
  y <- rnorm(100)
  expect_error(entropy_kde2d(x, y, n_grid = 0), 
               "n_grid must be greater than 1")
})

# Test 7: Check if the function returns a reasonable entropy value
test_that("entropy_kde2d returns a reasonable entropy value", {
  set.seed(123)
  n <- 1000
  mean <- c(0, 0)
  sd_x <- 1
  sd_y <- 5
  correlation <- 0.6
  sigma <- matrix(
    c(sd_x^2, correlation * sd_x * sd_y,
      correlation * sd_x * sd_y, sd_y^2),
    ncol = 2
  )
  simulated_data <- mvrnorm(n, mu = mean, Sigma = sigma)
  x <- simulated_data[, 1]
  y <- simulated_data[, 2]
  result <- entropy_kde2d(x, y, n_grid = 50)
  expect_type(result, "double")
  expect_gt(result, 0)
})
