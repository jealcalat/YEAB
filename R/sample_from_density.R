#' Sample from a density estimate
#'
#' @param x A numeric variable from a (un)known distribution
#' @param n Number of samples to return
#'
#' @return A sample with distribution close to x
#' @export
#'
#'
#' @examples
#'
#' x <- rnorm(1000)
#' y <- sample_from_density(x, 1000)
#'
#' hist(x,
#'   breaks = 30,
#'   freq = FALSE,
#'   col = "grey",
#'   main = "Original data"
#' )
#'
sample_from_density <- function(x, n) {
  # Verify the input vector isn't empy or has NA values
  if (length(x) == 0 || any(is.na(x))) {
    stop("Input data must be non-empty and non-NA.")
  }
  
  # Verify there is at least two unique values
  if (length(unique(x)) == 1 & length(x) > 1) {
    stop("All values in the input data are identical. Cannot estimate density.")
  }
  
  # Verify there is at least two values to estimate density
  if (length(x) < 2) {
    stop("At least two values are required to estimate density.")
  }
  
  # pdf_x <- density(x, n = n^2)
  pdf_x <- ks::kde(x, gridsize = n * 2)
  # pdf_x$y <- pdf_x$estimate
  # pdf_x$xev <- pdf_x$eval.points
  approx(
    cumsum(pdf_x$estimate) / sum(pdf_x$estimate),
    pdf_x$eval.points,
    runif(n)
  )$y
}
