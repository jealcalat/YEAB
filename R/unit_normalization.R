#' Min-max normalization (also feature rescaling)
#'
#' @param x numeric, vector of values to rescale
#'
#' @return A numeric vector rescaled in the range \eqn{x' \in [0, 1]}
#' @export
#'
#' @examples
#' x <- 5:100
#' x_scaled <- unit_normalization(x)
#' x_scaled
unit_normalization <- function(x) {
  if (!is.numeric(x)) {
    stop("Input 'x' must be numeric.")
  }

  x_min <- min(x)
  x_max <- max(x)

  # Evitar división por cero si el máximo y mínimo son iguales
  if (x_max == x_min) {
    return(rep(0, length(x)))  # Retorna 0 en lugar de NaN
  }

  (x - x_min) / (x_max - x_min)
}


#' Normalization (or rescaling) between arbitrary a and b
#'
#' @param x numeric
#' @param a numeric
#' @param b numeric
#'
#' @return A numeric vector rescaled in the range \eqn{x' \in [a, b]}
#' @export
#' @examples
#' x <- 5:100
#' a <- 0
#' b <- 1
#' x_scaled <- ab_range_normalization(x, a, b)
#' x_scaled
#' a <- 100
#' b <- 1000
#' x_scaled <- ab_range_normalization(x, a, b)
#' x_scaled
ab_range_normalization <- function(x, a, b) {
  if (!is.numeric(x) || !is.numeric(a) || !is.numeric(b)) {
    stop("Inputs 'x', 'a', and 'b' must be numeric.")
  }

  x_min <- min(x)
  x_max <- max(x)

  # Evitar división por cero si el máximo y mínimo son iguales
  if (x_max == x_min) {
    return(rep((a + b) / 2, length(x)))  # Retorna el valor medio en lugar de NaN
  }

  a + (x - x_min) * (b - a) / (x_max - x_min)
}
