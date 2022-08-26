plot_colorByDensity <- function(x1, x2,
                                col_palette = 'jet',
                                ylim = c(min(0, x2), max(x2)),
                                xlim = c(min(0, x1), max(x1)),
                                xlab = "", 
                                ylab = "", 
                                main = "") {
  df <- data.frame(x1, x2)
  x <- densCols(x1, x2, colramp = colorRampPalette(c("black", "white")))
  df$dens <- col2rgb(x)[1, ] + 1L
  
  if (col_palette == 'jet') {
    cols <- colorRampPalette(c("#00007F", 
                               "blue", 
                               "#007FFF", 
                               "cyan", 
                               "#7FFF7F", 
                               "yellow", 
                               "#FF7F00", 
                               "red", 
                               "#7F0000"))(256)
  } else {
    cols <- hcl.colors(256, col_palette)
  }
  
  df$col <- cols[df$dens]
  plot(x2 ~ x1,
    data = df[order(df$dens), ],
    ylim = ylim,
    xlim = xlim,
    pch = 20,
    col = col,
    cex = 0.7,
    xlab = xlab,
    ylab = ylab,
    main = main,
    axes = F,
    xaxs="i", yaxs="i"
  )
  axis(1, seq(0, xlim[2], 10), tck = -0.015)
  axis(2, at = seq(0, ylim[2], 10), tck = -0.015, las = 2)
  box(lwd = 2)
  df$col
}
# 
# plot_colorByDensity(dftmp$hx.m, dftmp$hy.m)
# 
#    # 
# # fun_den <- function() {
# #   xm <- get('xm', envir = parent.frame(1))
# #   ym <- get('ym', envir = parent.frame(1))
# #   z  <- get('dens', envir = parent.frame(1))
# #   colramp <- get('colramp', parent.frame(1))
# #   fields::image.plot(xm, ym, z, 
# #                      col = colramp(256), 
# #                      legend.only = T, 
# #                      add =F)
# # }
# # 
# # smoothScatter(dftmp$hx.m, dftmp$hy.m,
# #               nrpoints = 0, 
# #               postPlotHook = fun_den)
# 
# 
# hm <- ggplot(dftmp, 
#              aes(x=hx.m,
#                  y=hy.m) ) +
#   stat_density_2d(aes(fill = ..density..), 
#                   geom = "raster", contour = FALSE) +
#   scale_x_continuous(expand = c(0, 0), 
#                      breaks = seq(0, 42.5, 10)) +
#   scale_y_continuous(expand = c(0, 0),
#                      breaks = seq(0, 28, 10)) +
#   labs(x = "X position (cm)",
#        y = "Y position (cm)") +
#   theme_few() +
#   theme(
#     legend.position='none'
#   ) +
#   scale_fill_gradientn(colours = hcl.colors(7))
# 
