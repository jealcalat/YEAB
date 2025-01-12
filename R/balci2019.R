#' @title Peak individual trial analysis using moving average
#'
#' @param tasa_norm, numeric, normalized response rate
#' @param bins, numeric
#'
#' @return a list with
#'  params: a numeric vector with start, stop, spread and argmax (the bin at which response rate is max)
#'  mov_av: the moving average
#' @export
#' @importFrom zoo rollapply
#' @details Based on Balci et al 2010
#' @examples
#' data("r_times")
#' # binarize r_times to create response rate at 2 sec bins
#' bins <- get_bins(r_times, 0, 180, 2)
#' bin_res <- 6
#' tasa <- f_table(bins, 0, 180, bin_res)
#' tasa_norm <- tasa$prop / max(tasa$prop)
#' bins <- tasa$bins
#' balci_ind <- balci2019(tasa_norm, bins)
#'
#' plot(bins, tasa_norm, xlab = "6 sec bins", )
#' lines(bins, balci_ind$mov_av, col = "blue", lwd = 2)
#' abline(v = balci_ind$params[c(1, 2, 4)], lwd = c(1, 1, 2), col = c(1, 1, "red4"))
balci2019 <- function(tasa_norm, bins) {
  # moving average
  mov_av <- zoo::rollapply(tasa_norm,
    width = 3,
    FUN = mean,
    align = "center",
    partial = TRUE
  )
  # find the peak
  max_rnorm <- max(mov_av)
  binmax <- bins[which.max(mov_av)]
  # find the start and stop of the peak
  start <- bins[bins < binmax & mov_av >= 0.7][1]
  stop <- bins[bins > binmax & mov_av < 0.7][1]
  spread <- stop - start

  list(params = c(start, stop, spread, binmax), mov_av = mov_av)
}
