
scatter_plot <- function(x, y, fill = "navy", color = "red", ...) {
  plot(
    x,
    y,
    axes = FALSE,
    panel.first = {
      axis(1, col = 8, tck = -0.015, cex = 1.5)
      axis(2, las = 1, col = 8, tck = -0.015, cex = 1.5)
      grid(col = 8)
    },
    pch = 21,
    cex = 1.2,
    col = color,
    bg = fill,
    ...
  )
}
