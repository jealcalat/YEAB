test_that("unit_normalization works correctly", {
  # Test 1: Check that the input is numeric
  expect_error(unit_normalization("string"), "Input 'x' must be numeric.")
  
  # Test 2: Check that the normalization is in the range [0, 1]
  x <- 5:100
  result <- unit_normalization(x)
  expect_true(all(result >= 0 & result <= 1))
  
  # Test 3: Check that the function works for a single value (should return 0)
  x_single <- 50
  result <- unit_normalization(x_single)
  expect_equal(result, 0)
  
  # Test 4: Check that the function handles equal values (should return 0s)
  x_equal <- rep(10, 5)
  result <- unit_normalization(x_equal)
  expect_true(all(result == 0))
})

test_that("ab_range_normalization works correctly", {
  # Test 1: Check that the input is numeric
  expect_error(ab_range_normalization("string", 0, 1), "Inputs 'x', 'a', and 'b' must be numeric.")
  
  # Test 2: Check that the normalization is in the range [a, b]
  x <- 5:100
  a <- 0
  b <- 1
  result <- ab_range_normalization(x, a, b)
  expect_true(all(result >= a & result <= b))
  
  # Test 3: Check that the function works for a single value (should return a constant in the range [a, b])
  x_single <- 50
  result <- ab_range_normalization(x_single, 0, 10)
  expect_equal(result, 5)
  
  # Test 4: Check that the function handles equal values (should return constant in the range [a, b])
  x_equal <- rep(10, 5)
  result <- ab_range_normalization(x_equal, 0, 100)
  expect_true(all(result == 50))
})
