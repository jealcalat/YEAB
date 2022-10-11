#' Peak individual trial analysis using moving average
#'
#' @param tasa_norm, numeric
#' @param bines, numeric
#'
#' @return a numeric vector with start, stop, spread and argmax (the bin at which response rate is max)
#' @export
#' @importFrom zoo rollapply
#' @details Based on Balci et al 2010
#' @examples
#'
balci2010 = function(tasa_norm, bines) {

  mov_av = zoo::rollapply(tasa_norm,
                     width = 3,
                     FUN = mean,
                     align = "center",
                     partial = TRUE)

  max_rnorm = max(mov_av)
  binmax = bines[mov_av == max_rnorm]

  start = bines[bines < binmax & mov_av >= 0.7][1]
  stop = bines[bines > binmax & mov_av < 0.7][1]
  spread = stop - start

  c(start, stop, spread,binmax)
}
