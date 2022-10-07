#' Break point using kmeans
#'
#' @param r_times
#' @param min_x
#' @param max_x
#' @param time_res
#'
#' @return data.frame
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom cluster clusGap
#' @examples
#' data("r_times")
#' bps_km <- bp_km(r_times, 0, 180, 2)
#' TODO: Error in dimnames(x) <- dn :
# length of 'dimnames' [2] not equal to array extent
bp_km <- function(r_times, min_x, max_x, time_res){

  x <- seq(min_x,max_x,time_res)
  r_times <- get_bins(r_times, min_x, max_x,time_res)
  resp_sec <- r_times %>%
    {
      bins <- .
      rate <- f_table(bins,min_x, max_x,time_res) # f_table is a custom fn
      rate
    }

  resp_sec <- data.frame(x = x, y = resp_sec)
  resp_sec_scaled <- apply(resp_sec, 2, scale) # clustering works better scaled
  colnames(resp_sec_scaled) <- c("x","y")
  set.seed(123)

  gap <- clusGap(resp_sec_scaled,pam,K.max = 4, B = 100) # gap statistic
  # to compute the optimal number of clusters btw 1 and 4
  # I restricted to 4 because of the nature of the data
  k <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method="Tibs2001SEmax")
  km <- pam(resp_sec_scaled, k)

  if(k == 3){

    s1 <- which(km$clustering == 2)[1]
    s2 <- last(which(km$clustering == 2)) # first last
    # s2 <- last(which(km$clustering[-s2] == 2)) # second last

  } else {

    s1 <- which(km$clustering == 2)[1]
    s2 <- last(which(km$clustering == 2))
  }

  if(k > 3){
    s3 <- which(km$clustering == 4)[1]
  } else {
    s3 <- 180
  }

  start  <- resp_sec[s1,1]
  stop   <- resp_sec[s2,2]
  middle <- (start + stop)/2
  spread <- stop - start

  r1 <- sum(r_times <= start)/start
  r2 <- sum(r_times > start & r_times <= stop)/spread

  r3 <- sum(r_times > stop)/(179 - stop)

  metrics <- data.frame(opt_k = k,
                        start = start,
                        stop = stop,
                        s3 = s3,
                        spread = spread,
                        middle = middle,
                        r1 = r1,
                        r2 = r2,
                        r3 = r3)

  metrics
}
