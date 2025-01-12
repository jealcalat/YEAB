test_that("gell_like works correctly", {
  # Test 1: Check that the function throws an error when n is odd
  expect_error(gell_like(7), "n should be even")
  
  # Test 2: Check that the output length is correct
  result <- gell_like(8)
  expect_equal(length(result), 8)
  
  # Test 3: Check that there are exactly n/2 zeros and n/2 ones
  result <- gell_like(8)
  expect_equal(sum(result == 0), 4)
  expect_equal(sum(result == 1), 4)
  
  # Test 4: Check that there are no more than three consecutive 0s or 1s
  result <- gell_like(8)
  for (i in 4:length(result)) {
    expect_true(sum(result[(i-3):i] == 0) < 4)
    expect_true(sum(result[(i-3):i] == 1) < 4)
  }
})
