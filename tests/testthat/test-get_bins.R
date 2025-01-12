library(testthat)

# Test 1: Basic functionality - Check binarization
test_that("get_bins binarizes correctly", {
  x <- 1:20
  result <- get_bins(x, 0, 20, 5)
  expected_result <- c(rep(5, 5), rep(10, 5), rep(15, 5), rep(20, 5))
  expect_equal(result, expected_result)
})

# Test 2: Small resolution - Check binarization with small resolution
test_that("get_bins handles small resolution", {
  set.seed(420)
  x <- sort(round(runif(16, 0, 5),2))
  result <- get_bins(x, 0, 5, 0.25)
  expected_result <- c(1.00, 1.00, 1.50, 1.75, 1.75, 2.00, 2.50, 2.75, 3.25, 3.25, 3.50, 3.75, 3.75, 4.25, 4.50, 5.00)
  expect_equal(result, expected_result)
})

# Test 3: Boundary cases - Check behavior at the boundaries of intervals
test_that("get_bins handles boundary values correctly", {
  x <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  result <- get_bins(x, 0, 20, 2)
  expected_result <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  expect_equal(result, expected_result)
})


# Test 4: NA values - Check behavior with NA values in x
test_that("get_bins handles NA values", {
  x <- c(0.1, 0.45, NA, 0.78, 1, 1.23, NA, 2.32, 2.54, 3.21, NA, 3.67, 4, 4.23, NA, NA)
  result <- get_bins(x, 0, 5, 0.2)
  expected_result <- c(0.2, 0.6,  NA, 0.8, 1.0, 1.4,  NA, 2.4, 2.6, 3.4,  NA, 3.8, 4.0, 4.4,  NA,  NA)
  expect_equal(result, expected_result)
})
