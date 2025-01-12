# File: test-fleshler_hoffman.R
library(testthat)
library(YEAB)  # Cambiar a tu nombre de paquete si es necesario

test_that("fleshler_hoffman returns expected interval values", {
  N <- 5
  VI <- 30
  intervals <- fleshler_hoffman(N, VI)
  
  expect_length(intervals, N)
  expect_true(all(intervals > 0), "All intervals should be greater than zero")
  expect_equal(intervals[N], VI * (1 + log(N)), tolerance = 1e-5)
})

test_that("fleshler_hoffman handles N = 1 correctly", {
  N <- 1
  VI <- 30
  intervals <- fleshler_hoffman(N, VI)
  
  expect_length(intervals, N)
  expect_equal(intervals[1], VI * (1 + log(N)), tolerance = 1e-5)
})

test_that("fleshler_hoffman returns error for invalid N or VI", {
  expect_error(fleshler_hoffman(-1, 30), "N must be a positive integer")
  expect_error(fleshler_hoffman(5, -10), "VI must be a positive number")
  expect_error(fleshler_hoffman("a", 30), "N must be a positive integer")
  expect_error(fleshler_hoffman(5, "b"), "VI must be a positive number")
})

test_that("fleshler_hoffman handles large values of N and VI", {
  N <- 1000
  VI <- 1000
  intervals <- fleshler_hoffman(N, VI)
  
  expect_length(intervals, N)
  expect_true(all(intervals > 0), "All intervals should be greater than zero")
})

test_that("fleshler_hoffman approximates the limit at n = N correctly", {
  N <- 10
  VI <- 50
  intervals <- fleshler_hoffman(N, VI)
  
  expect_equal(intervals[N], VI * (1 + log(N)), tolerance = 1e-5)
})
