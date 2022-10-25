# overlap_histogram

#' Overlap two histograms
#'
#' @param var1
#' @param var2
#' @param name1
#' @param name2
#' @param breaks
#' @param legend
#' @param main0
#' @param alpha0
#' @param grey
#' @param border
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' var1 <- rnorm(100)
#' var2 <- rexp(100)
#' hist_over(var1, var2, name1 = expression(N(mu == 0, sigma == 1)), name2 = expression(exp(lambda == 1)), grey = T)
hist_over <- function(var1, var2,
                      name1 = "",
                      name2 = "",
                      breaks = 30,
                      legend = T,
                      main0 = "",
                      alpha0 = 0.5,
                      grey = 0,
                      border = T, ...) {
  hist0 <- function(..., col = "skyblue",
                    border = T) {
    hist(...,
      col = col,
      border = border,
      xlab = "",
      ylab = "",
      axes = F
    )
  }

  library(scales)
  library(magrittr)

  colh <- c(
    rgb(0, 0, 1, 1, maxColorValue = 1),
    rgb(255 / 255, 255 / 255, 255 / 255, alpha0)
  )
  if (grey) colh <- c(alpha(grey(0.1, alpha0)), alpha(grey(0.9, alpha0)))

  max0 <- max(var1, var2)
  min0 <- min(var1, var2)

  breaks <- seq(min0, max0, l = breaks + 1)

  cont1_max <- hist(var1, breaks = breaks, plot = F)$counts %>% max()
  cont2_max <- hist(var2, breaks = breaks, plot = F)$counts %>% max()

  cont_max <- max(cont1_max, cont2_max) * 1.2
  var1 %>% hist0(
    xlim = c(min0, max0), breaks = breaks,
    freq = T, col = colh[1], ylim = c(0, cont_max),
    main = main0, border = border, ...
  )
  var2 %>% hist0(
    xlim = c(min0, max0), breaks = breaks,
    freq = T, col = colh[2], ylim = c(0, cont_max),
    add = T, border = border, ...
  )
  axis(1, lwd = 0, lwd.tick = 0.8, tck = -0.02, cex.axis = 0.8, xaxs = "i")
  axis(2, lwd = 0, lwd.tick = 0.8, tck = -0.02, cex.axis = 0.8)
  # axis(3, lwd = 0, lwd.tick = 0.8, tck = -0.02, lab = F)
  # axis(4, lwd = 0, lwd.tick = 0.8, tck = -0.02, lab = F)
  box(lwd = 0.5)
  if (legend) {
    # legend(min0,cont_max, legend = c(
    #   ifelse(nchar(name1)==0,substitute(var1) %>% deparse,name1),
    #   ifelse(nchar(name2)==0,substitute(var2) %>% deparse,name2),
    #   "Overlap"), fill = c('white','white', colh[1]), bty = "n", cex=1,ncol=3)

    legend(min0, cont_max,
      legend = c(
        ifelse(nchar(name1) == 0, substitute(var1) %>% deparse(), name1),
        ifelse(nchar(name2) == 0, substitute(var2) %>% deparse(), name2)
      ),
      fill = c(colh, colh[2]), bty = "n", cex = 1, ncol = 3
    )
  }
}
