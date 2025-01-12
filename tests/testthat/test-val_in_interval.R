# Test 1: Check if the value is inside the intervals
test_that("val_in_interval detects values inside intervals", {
  df <- data.frame(lower = c(1, 5, 10), upper = c(3, 8, 15))
  result <- val_in_interval(df, "lower", "upper", 6)
  expect_equal(result, c(2, 1, 0)) # The value 6 is above the first, inside the second, and below the third interval.
})

# Test 2: Check if the value is below all intervals
test_that("val_in_interval detects values below intervals", {
  df <- data.frame(lower = c(1, 5, 10), upper = c(3, 8, 15))
  result <- val_in_interval(df, "lower", "upper", 0)
  expect_equal(result, c(0, 0, 0)) # The value 0 is below all intervals.
})

# Test 3: Check if the value is above all intervals
test_that("val_in_interval detects values above intervals", {
  df <- data.frame(lower = c(1, 5, 10), upper = c(3, 8, 15))
  result <- val_in_interval(df, "lower", "upper", 20)
  expect_equal(result, c(2, 2, 2)) # The value 20 is above all intervals.
})

# Test 4: Check if the value is at the boundary of intervals
test_that("val_in_interval detects values at interval boundaries", {
  df <- data.frame(lower = c(1, 5, 10), upper = c(3, 8, 15))
  result <- val_in_interval(df, "lower", "upper", 3)
  expect_equal(result, c(2, 0, 0)) # The value 3 is at the upper boundary of the first interval and outside the others.
})

# Test 5: Check behavior when intervals are inverted
test_that("val_in_interval handles inverted limits", {
  df <- data.frame(lower = c(3, 8, 5), upper = c(1, 5, 10)) # Intervals with inverted limits
  result <- val_in_interval(df, "lower", "upper", 6)
  expect_equal(result, c(2, 1, 1)) # The function should correctly sort limits and evaluate the value.
})

# Test 6: Check behavior with an empty data frame
test_that("val_in_interval handles empty data frame", {
  df <- data.frame(lower = numeric(0), upper = numeric(0))
  result <- val_in_interval(df, "lower", "upper", 5)
  expect_equal(result, numeric(0)) # The result should be an empty vector for an empty data frame.
})

# Test 7: Check behavior with NA values in the data frame
test_that("val_in_interval handles NA values", {
  df <- data.frame(lower = c(1, NA, 10), upper = c(3, 8, NA))
  result <- val_in_interval(df, "lower", "upper", 6)
  expect_equal(result, c(2, NA, NA)) # Rows with NA in the interval should return NA in the result.
})
