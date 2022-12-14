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
#' x <- rnorm(1000, 0, 1)
#' set.seed(12)
#' y <- rnorm(1000, 0, 5)
#'
#' cov_matr <- cov(cbind(x, y))
#' sigmas <- diag(cov_matr)
#' det_sig <- prod(sigmas)
#' # equivalentemente: det_sig <- det(cov(cbind(x,y)))
#' # de acuerdo con https://en.wikipedia.org/wiki/Multivariate_normal_distribution#'Differential_entropy
#'
#' normal_entropy <- function(k, pi, det_sig) {
#'   # todo lo de la izquierda es una constante; en Maei y Frankland se elimina
#'   (k / 2) * (1 + log(2 * pi)) + (1 / 2) * log(det_sig)
#' }
#'
#' entropia <- normal_entropy(k = 2, pi = pi, det_sig)
#' entropia # retorna 4.397477
#' entropy_kde2d(x, y, n_grid = 50)
#' # retorna 4.376377
#' # ver https://stats.stackexchange.com/questions/63447/integrating-kernel-density-estimator-in-2d
#'
#' # another implementation ----
#' entropy_kde2d_v2 <- function(x, y, n_grid = 150) {
#'   # range of x and y datapoints
#'   xrange <- range(x)
#'   yrange <- range(y)
#'   # size of cell to evaluate; for x this is the x_lower and x_upper divided by
#'   # the number of points (same for y); gives an area of evaluation.
#'   cell_size <- (diff(xrange) / n_grid) * (diff(yrange) / n_grid)
#'   kde_xy <- kde2d(
#'     x, y,
#'     # select bandwith
#'     h = c(
#'       width.SJ(x,
#'         nb = n_grid,
#'         lower = xrange[1],
#'         upper = xrange[2],
#'         method = "dpi"
#'       ),
#'       width.SJ(y,
#'         nb = n_grid,
#'         lower = yrange[1],
#'         upper = yrange[2],
#'         method = "dpi"
#'       )
#'     ),
#'     lims = c(xrange, yrange),
#'     n = n_grid
#'   )
#'   nc <- sum(kde_xy$z) * cell_size
#'   z <- kde_xy$z * cell_size
#'   pz <- z / nc
#'   integrand <- (-1) * pz * log(pz)
#'   sum(integrand)
#' }
#' entropy_kde2d_v2(x, y, 50)
#' entropy_kde2d(x, y, n_grid = 50)
#'
#' #
#' library(ks)
#' set.seed(1)
#' n <- 10000
#' x <- rnorm(n, 0, 4)
#' y <- rnorm(n, 0, 8)
#' xy <- cbind(x, y)
#'
#' xmin <- min(x)
#' xmax <- max(x)
#' dx <- .1
#'
#' ymin <- min(y)
#' ymax <- max(y)
#' dy <- .1
#'
#' pts.x <- seq(xmin, xmax, dx)
#' pts.y <- seq(ymin, ymax, dy)
#' pts <- as.data.frame(expand.grid(x = pts.x, y = pts.y))
#' f_kde <- kde(xy, gridsize = 150)
#'
#' dx <- diff(f_kde$eval.points[[1]])[1]
#' dy <- diff(f_kde$eval.points[[2]])[1]
#'
#' pz_est <- f_kde$estimate
#' ?kde
#' sum(pz_est) * (dx * dy)
#' pz <- pz_est[pz_est > 0]
#' intg <- (-1) * pz * log(pz)
#' sum(intg) * dx * dy
#'
entropy_kde2d <- function(x, y, n_grid = 150) {
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
