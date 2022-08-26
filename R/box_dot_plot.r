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

  rm(.Random.seed, envir = globalenv())

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