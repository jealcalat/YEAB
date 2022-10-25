#' Count between intervals
#'
#' @param x
#' @param intervals
#' @return
#' @export
#' @importFrom dplyr lag
#' @importFrom dplyr between
#' @examples
n_between_intervals <- function(x, intervals, time_in) {
  # matrix of intervals min, max
  mat_x <- matrix(c(lag(intervals, 1), intervals), ncol = 2)
  mat_x <- mat_x[2:nrow(mat_x), ]
  rsp <- c()

  for (i in 1:nrow(mat_x)) {
    which_resp <- x[between(time_in, mat_x[i, 1], mat_x[i, 2])]
    if (length(which_resp)) {
      rtm <- max(which_resp)
      rsp <- c(rsp, rtm)
    } else {
      rsp <- c(rsp, 0)
    }
  }
  rsp
}
