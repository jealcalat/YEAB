% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/den_histogram.R
\name{den_histogram}
\alias{den_histogram}
\title{Histogram with marginal densities in R base plot}
\usage{
den_histogram(
  x_dat,
  side = 1,
  fill = "#B3B3FF",
  color = "red",
  plot_limits = NULL,
  density_limits = NULL,
  amplify_max_density = 1.1,
  density_line = TRUE,
  add = FALSE,
  axis = TRUE
)
}
\arguments{
\item{side}{integer}

\item{fill}{character}

\item{color}{character}

\item{plot_limits}{numeric, NULL by default}

\item{density_limits}{numeric, NULL by default}

\item{amplify_max_density}{numeric, 1.1 by default}

\item{density_line}{logical, TRUE by default}

\item{add}{logical, should a plot be added? FALSE by default}

\item{axis}{logical, should axis be added? TRUE by default}

\item{x}{numeric}
}
\description{
Histogram with marginal densities in R base plot
}
\examples{
x <- rnorm(100)
den_histogram(x)
}
