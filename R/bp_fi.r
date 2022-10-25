#' Single breakpoint algorithm, as the one used in Guilhardi & Church 2004
#' Works in fixed interval trials.
#'
#' @param r_times numeric, the times at which a response was emitted in a trial
#' @param if_val numeric, the duration of the IF interval
#'
#' @return A data frame of 3 columns
#'   bp: a numeric value which corresponds to the time at which a break point was detected
#'   r1: a numeric value of the response rate _before_ the breakpoint
#'   r2: a umeric value of the responser rate _after_ the breakpoint
#' @details This algorithm performs an extensive search of every combination (t1, t2)
#'   where t1 starts in the first response throw (length(r_times) - 1)
#'
#' @export
#'
#' @examples
#' data("r_times")
#' r_times <- r_times[r_times < 60]
#' single_bp <- bp_fi(r_times, 60)
#' par(las = 1)
#' plot(r_times, seq_along(r_times),
#'   xlim = c(0, max(r_times)),
#'   main = "Cummulative Record",
#'   xlab = "Time (s)",
#'   ylab = "Cum Resp",
#'   col = 2, type = "s"
#' )
#' abline(v = single_bp$bp)
bp_fi <- function(r_times, if_val) {
  if (max(r_times) < if_val) {
    trial_duration <- if_val
  } else {
    trial_duration <- max(r_times)
  }

  nr <- length(r_times)
  r <- nr / trial_duration
  A <- numeric(nr - 1)

  for (j in 1:(nr - 1)) {
    t1 <- r_times[j]
    t2 <- trial_duration - t1
    r1 <- sum(r_times <= t1) / t1
    r2 <- sum(r_times > t2) / t2
    A[j] <- sum(t1 * abs(r1 - r), t2 * abs(r2 - r), na.rm = T)
  }

  argmax_A <- which.max(A)
  bp <- r_times[argmax_A]
  r1 <- sum(r_times <= bp) / bp
  r2 <- sum(r_times > bp) / (trial_duration - bp)

  data.frame(bp = bp, r1 = r1, r2 = r2, d1 = t1, d2 = t2)
}
