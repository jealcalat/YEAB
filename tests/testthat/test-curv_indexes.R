# Load the testthat package
library(testthat)

# Simulate data similar to r_times using rnorm
set.seed(420)
sim_r_times <- rnorm(100, mean = 60, sd = 10)
sim_r_times <- sort(sim_r_times[sim_r_times < 60])  # Filter and sort values

# Create cumulative response data
sim_cr <- seq_along(sim_r_times)

# Test for curv_index_int function
test_that("curv_index_int returns a numeric value within expected range", {
  index <- curv_index_int(sim_cr, sim_r_times)
  
  expect_type(index, "double")        # Check if the result is numeric
  expect_true(index >= 0 && index <= 1) # Check if the index is within [0, 1]
})

# Test for curv_index_fry function
test_that("curv_index_fry returns a numeric value within expected range", {
  fi_val <- 60
  n_intervals <- 4
  index_fry <- curv_index_fry(sim_cr, sim_r_times, fi_val, n_intervals)
  
  expect_type(index_fry, "double")    # Check if the result is numeric
  expect_true(index_fry >= 0 && index_fry <= 1) # Check if the index is within [0, 1]
})

# Additional edge case tests
test_that("curv_index_int handles empty input", {
  expect_error(curv_index_int(numeric(0), numeric(0)), 
               "Inputs 'cr' and 'time_in' cannot be empty")
})

test_that("curv_index_fry handles empty input", {
  expect_error(curv_index_fry(numeric(0), numeric(0), fi_val, n_intervals), 
               "Inputs 'cr' and 'time_in' cannot be empty")
})

test_that("curv_index_fry returns expected value for simple case", {
  simple_cr <- c(1, 2, 3, 4)
  simple_time_in <- c(10, 20, 30, 40)
  simple_index_fry <- curv_index_fry(simple_cr, simple_time_in, fi_val = 40, n = 2)
  
  expect_true(simple_index_fry >= 0 && simple_index_fry <= 1)
})
