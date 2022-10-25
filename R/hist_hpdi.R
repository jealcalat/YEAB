
hist_hpdi <- function(samples,
                      ylab = "",
                      xlab = "",
                      fill_hist = grey(0.5),
                      percent = 90) {
  if (require(rethinking)) {
    densidad <- density(samples, adjust = 0.8, n = 1000)
    hist(samples,
      col = fill_hist,
      border = fill_hist,
      breaks = "fd",
      ylab = ylab,
      xlab = xlab,
      freq = FALSE,
      axes = FALSE,
      ylim = c(0, max(densidad$y) * 1.2)
    )
    axis(1)
    # Calcular HDPI al percent
    high_dpi <- HPDI(samples, prob = percent / 100)
    yd <- which(round(densidad$x, 2) == round(high_dpi, 2))
    x <- densidad$x[yd[1]:yd[length(yd)]]
    x <- c(x[1], x, densidad$x[yd[length(yd)]])
    y <- c(0, densidad$y[yd[1]:yd[length(yd)]], 0)
    polygon(
      x = x, y,
      col = grey(0.8, 0.8),
      border = grey(0.8, 0.8)
    )
    lines(densidad, lwd = 2, col = "navyblue")
    abline(v = c(x[1], tail(x, 1)), lty = 2, lwd = 2)
    arrows(
      x0 = high_dpi[1],
      x1 = high_dpi[2],
      y0 = max(densidad$y) * 1.02,
      y1 = max(densidad$y) * 1.02,
      lwd = 2,
      col = "navyblue",
      code = 3,
      length = 0.1
    )
    text(
      x = mean(high_dpi),
      y = max(densidad$y) * 1.1,
      labels = "HPDI 90%"
    )
  } else {
    message("Please install rethinking package.")
  }
}
