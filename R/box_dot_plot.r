#' Custom box plot with dots of y ~ x on top of boxplot and stripchart functions
#'
#' @param variable Numeric
#' @param groups chr or factor
#' @param xlab chr, the name of x axis
#' @param ylab crh, the name of y axis
#' @param col_pal chr vector, colors of the boxes; if none, a color palette is provided by Polychrome
#' @param sort_col_pal logical, should the col_pal be sorted?
#' @param fill chr, a color to fill the box
#' @param lty integer or chr, linetype of the median (1 by default)
#' @param boxlty integer or chr, linetype of the box  (1 by default)
#' @param whisklty integer or chr, linetype of the whiskers  (2 by default, dotted)
#' @param box_around logical, wheter to draw a box or just the axes
#'
#' @return
#' @importFrom Polychrome createPalette
#' @export
#' @examples
#' rxnx <- data.frame(var = rnorm(30), grupo = rep(c('A', 'B', 'C'), each = 10))
#' box_dot_plot(rxnx$var, rxnx$grupo, col_pal = 'none', lty =1, boxlty = 2, box_around = FALSE)
box_dot_plot <- function(variable,
                         groups,
                         xlab = "",
                         ylab = "",
                         col_pal,
                         sort_col_pal = T,
                         fill = "white",
                         lty = 1,
                         boxlty = 1,
                         whisklty = 2,
                         box_around = T) {
  if (col_pal[1] == "none") {
    set.seed(123)
    col_pal <- Polychrome::createPalette(15, c("#DF536B", "#9E9E9E"),
      "N",
      range = c(20, 60),
      target = "normal"
    )
  }

  if (sort_col_pal) {
    col_pal <- col_pal[sample.int(length(col_pal))]
  }

  boxplot(variable ~ groups,
    col = fill,
    outline = FALSE,
    boxcol = col_pal,
    whiskcol = col_pal,
    outcol = col_pal,
    outcex = 0.8,
    lty = lty,
    whisklty = whisklty,
    staplelty = 2,
    staplecol = col_pal,
    boxlty = boxlty,
    medlwd = 2,
    medcol = "#444444",
    lwd = 2,
    pch = 16,
    axes = F,
    xlab = xlab,
    ylab = ylab
  )
  stripchart(variable ~ groups,
    vertical = TRUE,
    method = "jitter",
    add = TRUE,
    pch = 21,
    cex = 0.8,
    col = col_pal
  )

  axis(1,
    tck = -0.01,
    labels = unique(groups),
    at = 1:length(unique(groups)),
    cex.axis = 1,
    lwd = 1.5
  )
  axis(2,
    las = 2, tck = -0.01,
    cex.axis = 0.8,
    lwd = 1.5
  )

  if (box_around) box(cex = 0.8)
}
