library(testthat)

# Test 1: Basic functionality - Check frequency table and proportions
test_that("f_table calculates frequencies and proportions correctly", {
  set.seed(420)
  min_x <- 0
  max_x <- 180
  bin_res <- 2
  x <- sort(round(runif(135, min_x, max_x),1))
  x <- get_bins(x, min_x, max_x, bin_res)
  result <- f_table(x, min_x, max_x, bin_res)
  expected_bins <-seq(min_x, max_x, bin_res)
  expected_freq <- c(
    0, 3, 1, 1, 0, 1, 0, 2, 1, 4, 2, 2, 1, 1, 1, 2, 2, 4, 4, 0,
    1, 1, 1, 2, 2, 0, 0, 2, 2, 1, 1, 2, 1, 3, 1, 2, 2, 1, 1, 0,
    4, 1, 2, 1, 4, 0, 1, 2, 3, 0, 2, 2, 0, 3, 2, 1, 0, 1, 2, 2,
    0, 3, 2, 2, 3, 3, 4, 2, 0, 0, 1, 2, 0, 2, 0, 2, 3, 0, 1, 2,
    5, 0, 3, 1, 0, 0, 0, 0, 3, 0, 0
  )
  expected_prop <- expected_freq / sum(expected_freq)
  
  expect_equal(result$bins, expected_bins)
  expect_equal(result$freq, expected_freq)
  expect_equal(result$prop, expected_prop)
})

# Test 2: Check zero-frequency bins
test_that("f_table includes zero-frequency bins", {
  x <- c(1, 2, 2, 6, 7)
  min_x <- 0
  max_x <- 10
  bin_res <- 2
  x <- get_bins(x, min_x, max_x, bin_res)
  result <- f_table(x, min_x, max_x, bin_res)
  
  expected_freq <- c(0, 3, 0, 1, 1, 0)  # Bins 0, 4 and 10 should have zero frequency
  expect_equal(result$freq, expected_freq)
})

# Test 3: Check handling of empty input
test_that("f_table handles empty input", {
  x <- numeric(0)  # Empty vector
  min_x <- 0
  max_x <- 10
  bin_res <- 2
  result <- f_table(x, min_x, max_x, bin_res)
  
  expected_bins <- seq(min_x, max_x, bin_res)
  expected_freq <- rep(0, length(expected_bins))  # All bins should have zero frequency
  expected_prop <- rep(NA, length(expected_bins)) # Proportions undefined
  
  expect_equal(result$bins, expected_bins)
  expect_equal(result$freq, expected_freq)
  expect_true(all(is.na(result$prop)))
})

# Test 4: Check handling of NA values
test_that("f_table handles NA values in x", {
  x <- c(0, 2, NA, 6)
  min_x <- 0
  max_x <- 10
  bin_res <- 2
  result <- f_table(x, min_x, max_x, bin_res)
  
  expected_freq <- c(1, 1, 0, 1, 0, 0)  # Bin 2 has one less due to NA
  expected_prop <- expected_freq / sum(expected_freq, na.rm = TRUE)
  
  expect_equal(result$freq, expected_freq)
  expect_equal(result$prop, expected_prop)
})


