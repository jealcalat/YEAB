scatter_marginal <- function(x, y, color, fill, ...) {
  # save original configuration
  old_op <- par()
  layout(
    matrix(c(2, 0, 1, 3),
      nrow = 2,
      ncol = 2,
      byrow = TRUE
    ),
    widths = c(3, 0.5),
    heights = c(0.5, 3), respect = TRUE
  )
  par(mar = c(5.1, 4.1, 0, 0))
  scatter_plot(x, y, fill = fill, color = color, ...)
  par(mar = c(0, 4.1, 0, 0), bty = "n")
  den_histogram(x, side = 1, axis = FALSE, fill = fill, color = color)
  par(mar = c(5.1, 0, 0, 0), bty = "n")
  den_histogram(y, side = 2, axis = FALSE, fill = fill, color = color)
  # restablecer config original
  # par(old_op)
}
