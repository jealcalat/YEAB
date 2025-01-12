library(testthat)

# Test 1: KL divergence of identical distributions should be close to 0
test_that("KL_div returns near zero for identical distributions", {
  set.seed(123)
  p <- rnorm(1000, mean = 0, sd = 1)
  q <- rnorm(1000, mean = 0, sd = 1)
  
  kl_value <- KL_div(p, q, -Inf, Inf)
  
  expect_true(abs(kl_value) < 0.1)
})

# Test 2: KL divergence should be positive for different distributions
test_that("KL_div returns positive value for different distributions", {
  set.seed(123)
  p <- rnorm(1000, mean = 0, sd = 1)
  q <- rnorm(1000, mean = 2, sd = 1)
  
  kl_value <- KL_div(p, q, -Inf, Inf)
  
  expect_true(kl_value > 0)
})

# Test 3: KL divergence between normal and uniform distributions
test_that("KL_div computes divergence between normal and uniform distributions", {
  set.seed(123)
  p <- rnorm(1000, mean = 0, sd = 1)
  y_min <- -5
  y_max <- 5
  q <- runif(1000, min = y_min, max = y_max)
  
  kl_value <- KL_div(p, q, y_min, y_max)
  
  expect_true(kl_value > 0)
})

# Test 4: Handle NA values in input vectors
test_that("KL_div handles NA values correctly", {
  set.seed(123)
  p <- c(rnorm(1000, mean = 0, sd = 1), NA, NA)
  q <- c(rnorm(1000, mean = 0, sd = 1), NA, 2)
  
  kl_value <- KL_div(p, q, -Inf, Inf)
  
  expect_true(is.numeric(kl_value))
})

# Test 5: Handle empty input vectors
test_that("KL_div handles empty input vectors", {
  expect_error(KL_div(numeric(0), numeric(0), -Inf, Inf),
               "Both input vectors 'x' and 'y' must have at least 2 points.")
  expect_error(KL_div(c(1), c(1), -Inf, Inf),
               "Both input vectors 'x' and 'y' must have at least 2 points.")
})

# Test 6: KL divergence with constant vectors
test_that("KL_div handles constant vectors correctly", {
  p <- rep(1, 100)
  q <- rep(1, 100)
  
  kl_value <- KL_div(p, q, 0, 2)
  
  # If both distributions are equally constant, KL divergence should be 0
  expect_true(abs(kl_value) < 1e-6)
  
  # Comparing with a different constant distribution
  q_diff <- rep(2, 100)
  kl_value_diff <- KL_div(p, q_diff, 0, 2)
  
  # KL divergence should be greater than 0
  expect_true(kl_value_diff > 0)
})

# Test 7: Integration limits finite vs infinite
test_that("KL_div handles finite and infinite integration limits", {
  set.seed(123)
  p <- rnorm(1000, mean = 0, sd = 1)
  q <- rnorm(1000, mean = 0, sd = 1)
  
  kl_infinite <- KL_div(p, q, -Inf, Inf)
  kl_finite <- KL_div(p, q, -3, 3)
  
  expect_true(abs(kl_infinite - kl_finite) < 0.1)
})

# Test 8: KL divergence should handle different sample sizes
test_that("KL_div handles different sample sizes", {
  set.seed(123)
  p <- rnorm(1000, mean = 0, sd = 1)
  q <- rnorm(500, mean = 0, sd = 1)
  
  kl_value <- KL_div(p, q, -Inf, Inf)
  
  expect_true(abs(kl_value) < 0.1)
})
