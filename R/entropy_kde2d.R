#' Shannon entropy in two dimensions
#'
#' @param x numeric, random vector
#' @param y numeric, random vector
#' @param n_grid numeric, number of grid cells to evaluate density
#' @importFrom MASS kde2d
#' @return A numeric value of the entropy in 2D
#' @export
#'
#' @examples
#' set.seed(123)
#' # Generate a 2D normal distribution with a correlation of 0.6
#' n <- 1000
#' mean <- c(0, 0)
#' sd_x <- 1
#' sd_y <- 5
#' correlation <- 0.6
#' sigma <- matrix(
#'   c(
#'     sd_x^2,
#'     correlation * sd_x * sd_y,
#'     correlation * sd_x * sd_y,
#'     sd_y^2
#'   ),
#'   ncol = 2
#' )
#' library(MASS)
#' simulated_data <- mvrnorm(n, mu = mean, Sigma = sigma)
#' x <- simulated_data[, 1]
#' y <- simulated_data[, 2]
#'
#' cov_matr <- cov(cbind(x, y))
#' sigmas <- diag(cov_matr)
#' det_sig <- prod(sigmas)
#' # According to https://en.wikipedia.org/wiki/Multivariate_normal_distribution#Differential_entropy:
#'
#' normal_entropy <- function(k, pi, det_sig) {
#'   # The left part is a constant;
#'   (k / 2) * (1 + log(2 * pi)) + (1 / 2) * log(det_sig)
#' }
#'
#' entropia <- normal_entropy(k = 2, pi = pi, det_sig)
#' print(entropia) # Should return a value close to 4.3997
#'
#' result <- entropy_kde2d(x, y, n_grid = 50)
#' print(result) # Should return a value close to 4.2177
#'
entropy_kde2d <- function(x, y, n_grid = 150) {
  if (length(x) == 0 || length(y) == 0) {
    stop("Input vectors 'x' and 'y' must have at least two elements.")
  }
  if (n_grid <= 1) {
    stop("n_grid must be greater than 1.")
  }
  # range of x and y datapoints
  xrange <- range(x)
  yrange <- range(y)
  # size of cell to evaluate; for x this is the x_lower and x_upper divided by
  # the number of points (same for y); gives an area of evaluation.
  cell_size <- (diff(xrange) / n_grid) * (diff(yrange) / n_grid)
  kde_xy <- kde2d(
    x, y,
    # select bandwith
    h = c(
      width.SJ(x,
        nb = n_grid,
        lower = xrange[1],
        upper = xrange[2],
        method = "dpi"
      ),
      width.SJ(y,
        nb = n_grid,
        lower = yrange[1],
        upper = yrange[2],
        method = "dpi"
      )
    ),
    lims = c(xrange, yrange),
    n = n_grid
  )
  # normalizing constant
  nc <- sum(kde_xy$z) * cell_size
  # get information vector
  info_vec <- -kde_xy$z * log(kde_xy$z)
  # get entropy and multiply by cell size; this is similar to integrate
  # over x and y, like integral(f(x, y)dxdy) on which dxdy is cell_size
  entropy <- (sum(info_vec) * cell_size) / nc
  entropy
}
