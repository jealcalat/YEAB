library(testthat)

# Test 1: Check if the function returns the correct number of samples
test_that("sample_from_density returns correct number of samples", {
  x <- rnorm(1000)
  samples <- sample_from_density(x, 500)
  expect_equal(length(samples), 500) # Should return exactly 500 samples
})

# Test 2: Check if the function handles empty input
test_that("sample_from_density handles empty input", {
  expect_error(
    sample_from_density(numeric(0), 100), 
    "Input data must be non-empty and non-NA."
  )
})

# Test 3: Check if the function handles NA values in input
test_that("sample_from_density handles NA values", {
  expect_error(
    sample_from_density(c(NA, NA, NA), 500), 
    "Input data must be non-empty and non-NA."
  )
})

# Test 4: Check if the function preserves the approximate distribution
test_that("sample_from_density preserves approximate distribution", {
  x <- rnorm(1000, mean = 5, sd = 2)
  samples <- sample_from_density(x, 1000)
  expect_true(abs(mean(samples) - 5) < 0.5) # Mean should be close to 5
  expect_true(abs(sd(samples) - 2) < 0.5)   # Standard deviation should be close to 2
})

# Test 5: Check if the function works with a small number of samples
test_that("sample_from_density works with small n", {
  x <- rnorm(100)
  samples <- sample_from_density(x, 5)
  expect_equal(length(samples), 5) # Should return exactly 5 samples
})

# Test 6: Check if the function handles all input values being identical
test_that("sample_from_density handles identical values", {
  expect_error(
    sample_from_density(c(3, 3, 3, 3), 50), 
    "All values in the input data are identical. Cannot estimate density."
  )
})

# Test 7: Check if the function handles input with a single value
test_that("sample_from_density handles single-value input", {
  expect_error(
    sample_from_density(c(7), 10), 
    "At least two values are required to estimate density."
  )
})
