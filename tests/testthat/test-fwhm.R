library(testthat)

# Test 1: Basic functionality with a normal distribution
test_that("fwhm calculates correctly for normal distribution", {
  set.seed(170)
  rx <- rnorm(100)
  den <- density(rx)
  
  fval <- fwhm(den$x, den$y)
  
  # Check if the returned values are numeric
  expect_true(is.numeric(fval$fwhm))
  expect_true(is.numeric(fval$peak))
  expect_true(is.numeric(fval$x1))
  expect_true(is.numeric(fval$x2))
  
  # Check if the fwhm value makes sense (it should not be negative)
  expect_true(fval$fwhm > 0)
  
  # Check if x1 and x2 are within the range of x values
  expect_true(fval$x1 >= min(den$x) && fval$x1 <= max(den$x))
  expect_true(fval$x2 >= min(den$x) && fval$x2 <= max(den$x))
})

# Test 2: Handle empty input (should throw an error)
test_that("fwhm handles empty input", {
  expect_error(fwhm(numeric(0), numeric(0)), 
               "Input vectors 'x' and 'y' cannot be empty")
})

# Test 3: Case with a single value (edge case)
test_that("fwhm handles single value input", {
  x <- c(1)
  y <- c(1)
  expect_error(fwhm(x, y), 
               "FWHM cannot be calculated for a distribution with no variation in y")
})

# Test 4: Test with an asymmetric distribution
test_that("fwhm handles asymmetric distributions", {
  set.seed(170)
  rx <- rexp(100, rate = 0.1)
  den <- density(rx)
  
  fval <- fwhm(den$x, den$y)
  
  # Check if the returned values are numeric
  expect_true(is.numeric(fval$fwhm))
  expect_true(is.numeric(fval$peak))
  expect_true(is.numeric(fval$x1))
  expect_true(is.numeric(fval$x2))
  
  # Check if the fwhm value makes sense (it should not be negative)
  expect_true(fval$fwhm > 0)
})


