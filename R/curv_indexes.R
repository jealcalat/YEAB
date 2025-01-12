#' Curvature index by numerical integration
#'
#' @param cr numeric, cumulative response
#' @param time_in numeric, time (or the x axis in a cumulative response plot)
#'
#' @return a numeric value that is the proportion of a rect triangle area minus
#'    the area under the curve
#' @export
#'
#' @importFrom sfsmisc integrate.xy
#'
#' @examples
#' data("r_times")
#' r_times <- r_times[r_times < 60]
#' cr <- seq_along(r_times)
#'
#' plot(r_times, cr, type = "s")
#' curv_index_int(cr, r_times)
#' segments(
#'   x0 = min(r_times), y0 = min(cr),
#'   x1 = max(r_times), y1 = max(cr)
#' )
#' segments(
#'   x0 = min(r_times) + (max(r_times) - min(r_times)) / 2, y0 = min(cr),
#'   x1 = max(r_times), y1 = max(cr),
#'   col = "red"
#' )
curv_index_int <- function(cr, time_in) {
  if (length(cr) == 0 || length(time_in) == 0) {
    stop("Inputs 'cr' and 'time_in' cannot be empty")
  }
  # Curvature index with numerical integration
  # Get the AUC using numerical integration with cubic splines interpolation

  # total area of a right triangle
  area0 <- max(time_in) * max(cr) / 2
  # integrate needs sfsmisc
  # minimum time (first response)
  a <- min(time_in)
  # maximum time (last response)
  b <- time_in[which.max(cr)]
  # area under the function f(t_response)
  area1 <- integrate.xy(time_in, cr, a, b)
  # difference of rect triangle and actual auc of f(t_response)
  index <- (area0 - area1) / area0
  return(index)
}
#' Curvature index using Fry derivation
#'
#' @param cr A numeric vector of cumulative response
#' @param fi_val the FI value
#' @param time_in numeric, time (or the x axis in a cumulative response plot)
#' @param n numeric, the number of subintervals
#'
#' @return The curvature index as exposed by Fry
#' @export
#'
#' @examples
#' data("r_times")
#' r_times <- r_times[r_times < 60]
#' cr <- seq_along(r_times)
#'
#' plot(r_times, cr, type = "s", xlim = c(min(r_times), max(r_times)))
#' segments(
#'   x0 = min(r_times), y0 = min(cr),
#'   x1 = max(r_times), y1 = max(cr)
#' )
#' segments(
#'   x0 = min(r_times) + (max(r_times) - min(r_times)) / 2, y0 = min(cr),
#'   x1 = max(r_times), y1 = max(cr),
#'   col = "red"
#' )
#' curv_index_fry(cr, r_times, 60, 4)
curv_index_fry <- function(cr, time_in, fi_val, n = 4) {
  if (length(cr) == 0 || length(time_in) == 0) {
    stop("Inputs 'cr' and 'time_in' cannot be empty")
  }
  # Curvature index using Fry method
  # Size of subintervals; eg., if n=4 and b=60, size are 15 sec bins
  size <- round(fi_val / n) # Aquí b se tendría que cambiar por fi_val
  # Subintervals generated evenly, from 0 to max(t)
  intervals <- seq(0, ceiling_multiple(max(time_in), fi_val), size)
  # Responses at each subinterval
  # resps <- cr[t %in% intervals]
  resps <- n_between_intervals(cr, intervals, time_in) # nolint
  indexFry <- ((n - 1) * resps[n] - 2 * sum(resps[1:(n - 1)])) / (n * resps[n])
  return(indexFry)
}



## test
#


# fry = curv_index_fry(cr=cumsum(df$y), t=df$x, 5, 99)
# numint = curv_index_int(cr=cumsum(df$y), t=df$x, 99)
#
#
# plot(time_in, cr, type = 's', xlim = c(0, 90))
# points(intervals[-1], resps, col = 'red', pch = 16)
#
# # segments(x0=intervals[-length(intervals)],
# #          x1=intervals[-1],
# #          y0=c(0,resps[-length(resps)]),
# #          y1=resps)
# # segments(x0=intervals[-1],
# #          x1=intervals[-1],
# #          y0=rep(0,length(resps)),
# #          y1=resps)
# segments(x0=0, y0=0,
#          x1=t[which.max(cr)],y1=max(cr), lty=2, lwd=2.5)
# segments(x0=t[which.max(cr)], y0=0,
#          x1=t[which.max(cr)],y1=max(cr), lty=2, lwd=2.5)
# segments(x0=0, y0=0, x1=t[which.max(cr)],y1=0, lty=2, lwd=2.5)
#
# segments(x0=b * numint,y0=0,x1=b,y1=max(cr), col = 'red')
# segments(x0=b * fry,y0=0,x1=b,y1=max(cr), col = 'blue')
