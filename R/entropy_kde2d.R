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

# probar

set.seed(123)
x <- rnorm(1000, 0, 1)
set.seed(12)
y <- rnorm(1000, 0, 5)

cov_matr <- cov(cbind(x, y))
sigmas <- diag(cov_matr)
det_sig <- prod(sigmas)
# equivalentemente: det_sig <- det(cov(cbind(x,y)))
# de acuerdo con https://en.wikipedia.org/wiki/Multivariate_normal_distribution#Differential_entropy

normal_entropy <- function(k, pi, det_sig) {
  # todo lo de la izquierda es una constante; en Maei y Frankland se elimina
  (k / 2) * (1 + log(2 * pi)) + (1 / 2) * log(det_sig)
}

entropia <- normal_entropy(k = 2, pi = pi, det_sig)
entropia # retorna 7.415341
entropy_kde2d(x, y, n_grid = 50)
# retorna 7.445117
# muy cercanos
#
## Más ejemplos

# https://stats.stackexchange.com/questions/63447/integrating-kernel-density-estimator-in-2d

library(MASS) # kde2d
library(spatstat) # im class
f <- function(xy, n, x, y, ...) {
  #
  # Estimate the total where the density does not exceed that at (x,y).
  #
  # `xy` is a 2 by ... array of points.
  # `n`  specifies the numbers of rows and columns to use.
  # `x` and `y` are coordinates of "probe" points.
  # `...` is passed on to `kde2d`.
  #
  # Returns a list:
  #   image:    a raster of the kernel density
  #   integral: the estimates at the probe points.
  #   density:  the estimated densities at the probe points.
  #
  xy.kde <- kde2d(xy[1, ], xy[2, ], n = n, ...)
  xy.im <- im(t(xy.kde$z), xcol = xy.kde$x, yrow = xy.kde$y) # Allows interpolation $
  z <- interp.im(xy.im, x, y) # Densities at the probe points
  c.0 <- sum(xy.kde$z) # Normalization factor $
  i <- sapply(z, function(a) sum(xy.kde$z[xy.kde$z < a])) / c.0
  return(list(image = xy.im, integral = i, density = z))
}
#
# Generate data.
#
n <- 256
set.seed(17)
xy <- matrix(c(
  rnorm(k <- ceiling(2 * n * 0.8), mean = c(6, 3), sd = c(3 / 2, 1)),
  rnorm(2 * n - k, mean = c(2, 6), sd = 1 / 2)
), nrow = 2)
#
# Example of using `f`.
#
y.probe <- 1:6
x.probe <- rep(6, length(y.probe))
lims <- c(min(xy[1, ]) - 15, max(xy[1, ]) + 15, min(xy[2, ]) - 15, max(xy[2, ] + 15))
ex <- f(xy, 200, x.probe, y.probe, lim = lims)
ex$density
ex$integral
#
# Compare the effects of raster resolution and bandwidth.
#
res <- c(8, 40, 200, 1000)
system.time(
  est.0 <- sapply(
    res,
    function(i) f(xy, i, x.probe, y.probe, lims = lims)$integral
  )
)
est.0
system.time(
  est.1 <- sapply(
    res,
    function(i) f(xy, i, x.probe, y.probe, h = 1, lims = lims)$integral
  )
)
est.1
system.time(
  est.2 <- sapply(
    res,
    function(i) f(xy, i, x.probe, y.probe, h = 1 / 2, lims = lims)$integral
  )
)
est.2
system.time(
  est.3 <- sapply(
    res,
    function(i) f(xy, i, x.probe, y.probe, h = 5, lims = lims)$integral
  )
)
est.3
results <- data.frame(
  Default = est.0[, 4], Hp5 = est.2[, 4],
  H1 = est.1[, 4], H5 = est.3[, 4]
)
#
# Compare the integrals at the highest resolution.
#
par(mfrow = c(1, 1))
panel <- function(x, y, ...) {
  points(x, y)
  abline(c(0, 1), col = "Red")
}
pairs(results, lower.panel = panel)
#
# Display two of the density estimates, the data, and the probe points.
#
par(mfrow = c(1, 2))
xy.im <- f(xy, 200, x.probe, y.probe, h = 0.5)$image
plot(xy.im, main = "Bandwidth=1/2", col = terrain.colors(256))
points(t(xy), pch = ".", col = "Black")
points(x.probe, y.probe, pch = 19, col = "Red", cex = .5)

xy.im <- f(xy, 200, x.probe, y.probe, h = 5)$image
plot(xy.im, main = "Bandwidth=5", col = terrain.colors(256))
points(t(xy), pch = ".", col = "Black")
points(x.probe, y.probe, pch = 19, col = "Red", cex = .5)

# yet another ---



entropy_kde2d_v2 <- function(x, y, n_grid = 150) {
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
  nc <- sum(kde_xy$z) * cell_size
  z <- kde_xy$z * cell_size
  pz <- z / nc
  integrand <- (-1) * pz * log(pz)
  sum(integrand)
}
entropy_kde2d_v2(x, y, 50)
entropy_kde2d(x, y, n_grid = 50)

#
library(ks)
set.seed(1)
n <- 10000
x <- rnorm(n, 0, 4)
y <- rnorm(n, 0, 8)
xy <- cbind(x, y)

xmin <- min(x)
xmax <- max(x)
dx <- .1

ymin <- min(y)
ymax <- max(y)
dy <- .1

pts.x <- seq(xmin, xmax, dx)
pts.y <- seq(ymin, ymax, dy)
pts <- as.data.frame(expand.grid(x = pts.x, y = pts.y))
f_kde <- kde(xy, gridsize = 150)

dx <- diff(f_kde$eval.points[[1]])[1]
dy <- diff(f_kde$eval.points[[2]])[1]

pz_est <- f_kde$estimate
?kde
sum(pz_est) * (dx * dy)
pz <- pz_est[pz_est > 0]
intg <- (-1) * pz * log(pz)
sum(intg) * dx * dy
