#' Reaction Times from Peak Procedure
#'
#' A dataset containing reaction times (in seconds) from an experiment using the peak procedure.
#'
#' @name r_times
#' @title Reaction Times from Peak Procedure
#' @format A numeric vector with 132 elements representing reaction times.
#' @details These times are derived from a peak procedure experiment, typically used in behavioral experiments to measure timing abilities in subjects.
#' @source Generated during a behavioral analysis experiment.
"r_times"

#' Simulated Data for Hyperbolic Discounting
#'
#' A list of simulated data for fitting hyperbolic discounting models.
#'
#' @name hyp_data
#' @title Simulated Data for Hyperbolic Discounting
#' @format A list with 3 elements:
#' \describe{
#'   \item{sv}{A numeric vector of normalized subjective values with added noise.}
#'   \item{delay}{A numeric vector of delays (in seconds).}
#'   \item{real_k}{The real value of the discounting parameter.}
#' }
#' @details This dataset was generated to simulate the behavior of a hyperbolic discounting function.
#' It is commonly used in behavioral economics and psychology to study delay discounting behaviors.
#' @source Generated using a custom simulation function.
"hyp_data"

#' Delay Discounting Data
#'
#' A dataset containing normalized subjective values (SV) and delays used in a delay discounting task.
#'
#' @name DD_data
#' @title Delay Discounting Data
#' @format A data frame with 6 rows and 2 columns:
#' \describe{
#'   \item{norm_sv}{Normalized subjective values (numeric).}
#'   \item{Delay}{Delays (in seconds) for rewards (numeric).}
#' }
#' @details This dataset represents results from a delay discounting experiment. It demonstrates how subjective values decay with increasing delays.
#' @source Generated for a delay discounting analysis.
"DD_data"

#' Raw Fixed Interval Data
#'
#' A dataset containing raw data from a fixed interval (FI) experiment.
#'
#' @name fi60_raw_from_med
#' @title Raw Fixed Interval Data
#' @format A character vector containing lines of data from the file.
#' @details This dataset is obtained from a raw file generated during an FI experiment. It provides raw, unprocessed behavioral data.
#' @source The raw data was read from the file: \code{inst/extdata/fi60_raw.txt}.
"fi60_raw_from_med"

#' Gaussian Example 1 Data
#'
#' This dataset contains a series of bins and corresponding response averages from another experimental task, similar to the first example.
#' 
#' @format A data frame with 91 rows and 2 columns:
#' \describe{
#'   \item{Bin}{Numeric. The bin number.}
#'   \item{Response_Average}{Numeric. The average response in each bin.}
#' }
#' @source Generated as part of a synthetic example for the task.
"gauss_example_1"

#' Gaussian Example 2 Data
#'
#' This dataset contains a series of bins and corresponding response averages, this time with a slightly different distribution and experimental task design.
#' 
#' @format A data frame with 91 rows and 2 columns:
#' \describe{
#'   \item{Bin}{Numeric. The bin number.}
#'   \item{Response_Average}{Numeric. The average response in each bin.}
#' }
#' @source Generated as part of a synthetic example for the task.
"gauss_example_2"

#' Gaussian Example Data
#'
#' This dataset contains a series of bins and corresponding response averages from an experimental task.
#' 
#' @format A data frame with 91 rows and 2 columns:
#' \describe{
#'   \item{Bin}{Numeric. The bin number.}
#'   \item{Response_Average}{Numeric. The average response in each bin.}
#' }
#' @source Generated as part of a synthetic example for the task.
"gauss_example"

