# File: tests/testthat/test-ceiling_multiple.R

test_that("ceiling_multiple returns the nearest multiple correctly", {
  # General cases
  expect_equal(ceiling_multiple(8, 10), 10)
  expect_equal(ceiling_multiple(12, 10), 20)
  expect_equal(ceiling_multiple(21, 11), 22)
  
  # Case where x is already a multiple of 'multiple'
  expect_equal(ceiling_multiple(20, 10), 20)
  
  # Edge cases
  expect_equal(ceiling_multiple(0, 5), 0)           # Zero as a multiple
  expect_equal(ceiling_multiple(-3, 5), 0)          # Negative number, next multiple is 0
  expect_equal(ceiling_multiple(-8, 5), -5)         # Next negative multiple
  expect_equal(ceiling_multiple(2.5, 1), 3.0)       # Decimal, with integer multiple
  expect_equal(ceiling_multiple(7.5, 2.5), 7.5)    # Decimal with decimal multiple
})
