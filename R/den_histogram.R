#' Histogram with marginal densities in R base plot
#'
#' @param x numeric
#' @param side integer
#' @param fill character
#' @param color character
#' @param plot_limits numeric, NULL by default
#' @param density_limits numeric, NULL by default
#' @param amplify_max_density numeric, 1.1 by default
#' @param density_line logical, TRUE by default
#' @param add logical, should a plot be added? FALSE by default
#' @param axis logical, should axis be added? TRUE by default
#'
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' den_histogram(x)
den_histogram <- function(x,
                          side = 1,
                          fill = "#B3B3FF",
                          color = "red",
                          plot_limits = NULL,
                          density_limits = NULL,
                          amplify_max_density = 1.1,
                          density_line = TRUE,
                          add = FALSE,
                          axis = TRUE) {
  # Basado en https://osf.io/bv8fr

  if (class(plot_limits) == "NULL") {
    plot_limits <- range(x)
  }

  x <- x[which(x >= plot_limits[1] & x <= plot_limits[2])]
  h <- hist(x, breaks = "fd", plot = FALSE)
  x_base <- rep(h$breaks, each = 2)
  y_base <- c(0, rep(h$density, each = 2), 0)
  breaks_base <- h$breaks
  densities_base <- c(h$density, tail(h$density, 1))
  # kernel density
  den <- KernSmooth::bkde(x)

  if (class(density_limits) == "NULL") {
    density_limits <- c(0, max(h$density) * amplify_max_density)
  }

  variable_limits <- plot_limits

  if (side == 1) {
    xlims <- variable_limits
    ylims <- density_limits
    x <- x_base
    y <- y_base
    x0 <- breaks_base
    x1 <- breaks_base
    y0 <- rep(0, length(breaks_base))
    y1 <- densities_base
    x_den <- den$x
    y_den <- den$y
  }
  if (side == 2) {
    xlims <- density_limits
    ylims <- variable_limits
    x <- y_base
    y <- x_base
    y0 <- breaks_base
    y1 <- breaks_base
    x0 <- rep(0, length(breaks_base))
    x1 <- densities_base
    axis <- FALSE
    y_den <- den$x
    x_den <- den$y
  }

  if (!add) {
    plot(0, type = "n", xlim = xlims, ylim = ylims, axes = FALSE, ann = FALSE)
    if (axis) {
      axis(1, tck = -0.015, lwd = 2, col = "#525252", cex = 2)
      axis(2, tck = -0.015, lwd = 2, col = "#525252", cex = 2, las = 2)
    }
    polygon(x, y, col = fill, border = NA)
    if (density_line) {
      lines(
        x_den, y_den, # from = plot_limits[1], to = plot_limits[2]),
        col = color, lwd = 2
      )
    }
  }
}
