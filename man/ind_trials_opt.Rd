% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ind_trials_opt.R
\name{ind_trials_opt}
\alias{ind_trials_opt}
\title{Find the best fit for individual trials using \code{optim}}
\usage{
ind_trials_opt(r_times, trial_duration, optim_method = "Nelder-Mead")
}
\arguments{
\item{r_times}{Vector of response times}

\item{trial_duration}{Duration of the trial}

\item{optim_method}{Optimization method. See \code{optim} for details.}
}
\value{
A data frame with the following columns:
\itemize{
\item \code{start}: The start time of the peak
\item \code{stop}: The stop time of the peak
\item \code{spread}: The spread of the peak (stop - start)
\item \code{middle}: The middle of the peak (mean of start and stop)
}
}
\description{
Find the best fit for individual trials by minimizing the
sum of areas between the response rate and the target rate.
}
\examples{
response_times <- c(28.1, 40.7, 44.2, 44.4, 44.7, 45, 45.4, 47.9, 48.1, 48.3,
  48.6, 48.8, 49.8, 50.2, 50.7, 51.2, 51.4, 51.7, 51.9, 52.7, 53, 53.5, 53.7,
  53.9, 54.1, 54.3, 54.9, 55.3, 55.5, 55.7, 55.8, 57.2, 57.4, 57.7, 58.3,
  58.5, 58.7, 60.4, 60.6, 60.7, 61.1, 61.6, 61.8, 62.6, 62.8, 63.1, 63.3,
  63.5, 63.8, 64.4, 64.8, 64.9, 65.1, 66.1, 66.4, 67, 68.7, 68.9, 69.5, 69.6,
  70.1, 70.9, 71, 71.3, 71.6, 71.8, 73.9, 74.1, 74.4, 74.6, 75.2, 76.4,
  76.6, 77.4, 77.6, 77.8, 78.2, 79.3, 79.9, 80.5, 80.7, 81.3, 82.2, 82.4,
  82.6, 82.9, 83, 83.1, 83.7, 84.4, 84.4, 84.8, 85, 85.6, 86.6, 87, 87.1,
  87.3, 87.4, 87.8, 88.1, 88.2, 89.4, 99.1, 99.3, 99.6, 99.8, 100.2,
  133.1, 133.1, 133.6, 134.9, 135.2, 135.3, 135.4, 135.7, 136.5, 173.8,
  174.1, 174.3, 174.7, 175.9, 176.3, 176.6, 177.4, 177.5, 177.7, 178.1,
  178.2, 178.4, 178.5, 178.8, 179.4)
# Replace with your own initial guess
initial_guess <- c(min(response_times), mean(response_times))
trial_duration <- max(response_times)
result <- ind_trials_opt(response_times, trial_duration)
par(mgp = c(2.3, 0.2, 0),
  mar = c(4, 4.5, 1, 1))
plot(
  density(
    response_times,
    adjust = 0.8,
    from = 0,
    to = trial_duration
  ),
  main = "Density plot of response times",
  xlab = "Response time (ms)",
  ylab = expression(italic(p(t[R]))),
)
abline(v = 60, lty = 2)
abline(v = result$start, col = "red")
abline(v = result$stop, col = "red")
abline(v = result$middle, col = "red")
}
