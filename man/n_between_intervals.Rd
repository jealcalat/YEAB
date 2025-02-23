% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_between_intervals.R
\name{n_between_intervals}
\alias{n_between_intervals}
\title{Find maximum value within intervals}
\usage{
n_between_intervals(x, intervals, time_in)
}
\arguments{
\item{x}{A numeric vector representing the distribution from which
to find the maximum value within intervals.}

\item{intervals}{A numeric vector specifying the intervals within
which to search for the maximum value.}

\item{time_in}{A numeric vector representing the corresponding time points
for the values in the vector x, which is used to determine whether
the values fall within the specified intervals.}
}
\value{
A numeric vector containing the maximum value within each
interval specified by 'intervals'. If no values fall within an interval,
returns 0 for that interval.
}
\description{
This function searches for the maximum value within a distribution
(represented by vector x) that falls within a series of intervals
specified by the vector intervals.
}
\examples{
# Create a vector of data with a logarithmically increasing distribution
log_data <- round(exp(seq(log(1), log(100), length.out = 100)))

# Define intervals to cover the range 1-100
intervals <- seq(1, 100, by = 20)

# Create a corresponding time vector
time_in <- seq(1, 100, length.out = 100)

# Find maximum value within intervals
n_between_intervals(log_data, intervals, time_in)
}
