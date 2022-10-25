# stem_hist makes histograms with vertical lines
# like those in David MacKay classic "Information theory, inference and learning"

stem_hist <- function(dat,
                      xlabel = "x",
                      ylabel = "counts",
                      lcolor = "black",
                      lwidth = 2,
                      ltype = 1,
                      breaks = "fd") {
  hx <- hist(dat, breaks = breaks, plot = F)
  par(mgp = c(1.2, 0.15, 0))
  plot(hx$mids, hx$counts,
    type = "h",
    xlab = xlabel,
    ylab = ylabel,
    lwd = lwidth,
    lty = ltype,
    col = lcolor,
    xlim = c(min(hx$breaks), max(hx$breaks)),
    ylim = c(0, max(hx$counts) * 1.1),
    axes = F,
    xaxs = "i",
    yaxs = "i"
  )
  axis(1, tck = 0.015, lwd = 2, lwd.ticks = 0.5)
  axis(2, las = 2, tck = 0.015, lwd.ticks = 0.5, lwd = 2)
  box()
}

# m = 10
# sd = 2

# xg = rnorm(1000,m,sd)

# stem_hist(xg,lcolor = 6, lwidth=1.5, ltype = 1)
