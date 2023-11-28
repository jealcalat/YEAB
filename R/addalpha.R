#' @title Convert colors to RGBA
#' Adds transparency to a list of colors
#'
#' @param cols integer or chr; if integer from 1-9 takes the value from palette(); if chr a named color from colors()
#' @param alpha numeric, specifying the opacity (1) or transparency (0)
#'
#' @return character vector of colors
#' @export
#' @importFrom scales show_col
#' @examples
#' my_red <- addalpha("red", 0.4)
#' plot(0, 0, col = addalpha("red", 0.1), pch = 16, cex = 2)
#' # more than one color at once
#' my_col_vec <- addalpha(c(1, 2, "red", "navy"), 0.4)
#' scales::show_col(my_col_vec)
addalpha <- function(cols, alpha) {
  # convert to rgb
  rgb(t(col2rgb(cols) / 255),
    alpha = alpha
  )
}
