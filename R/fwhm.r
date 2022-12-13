# fwhm.R
# Full width half maximum and maximum at full width.
# The arguments x and y come from the output
# of the density() function, i.e, x = density()$x, y = density()$y

#' Full Width at Half Maximum
#'
#' @param x numeric, a vector of values from a distribution (density)
#' @param y numeric, a vector of probabilities
#'
#' @return a list with the fwhm and the x at which the max ocurred
#' @export
#' @details The function allows to compute the spread of a symmetric function
#' even when it is not normally distributed. It first finds the x at which y is max,
#' then
#' x1 and x2 can be recovered using x1=peak-fwhm/2, x2=peak+fwhm/2
#' @examples
#' set.seed(170)
#' rx <- rnorm(100)
#' den <- density(rx)
#' fval <- fwhm(den$x, den$y)
#' x1 <- fval$peak - fval$fwhm / 2
#' x2 <- fval$peak + fval$fwhm / 2
#' plot(den)
#' abline(v = c(x1, fval$peak, x2), col = c(1, 2, 1))
#'
fwhm <- function(x, y) {
  xy <- data.frame(x = x, y = y)
  xmax <- xy$x[which.max(den$y)][1] # thakes just one max if there were 2
  x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y) / 2))]
  x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y) / 2))]
  fwhm <- x2 - x1
  list(fwhm = fwhm, peak = xmax)
}
