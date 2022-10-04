# seq table freq, crea tabla de frecuencia

# discretize time in bins of custom resolution
# Input:
#       x: numeric vector to be discretized (or binned)
#       x_min: where to start (e.g., from 0)
#       x_max: where to stop (e.g., 60)
#       res: resolution of bins (e.g., every 2 seconds)
# Output:
#       a vector of bins with intervals 'res'
get_bins <- function(x, x_min, x_max, res) {
  # Create a sequence of intervals at wich x will be binned, taking pairs of values in order
  # eg, (0, 10, 20) will form paris of intervals of (0, 10], (10, 20] and (10, 20]
  seq_intervals <- seq(x_min, x_max, res)
  # Find intervals takes x and makes intervals according to the sequence above.
  # If x = 1, 4, 9, 10, 12 and seq_intervals = 0, 10, 20 (bins of 10)
  # this will return clases of 1 1 1 1 2. The last part, "* res", multiply the class intervals
  # the return the true bins, so  1 1 1 1 2 -> 10 10 10 10 20
  findInterval(x, seq_intervals, left.open = TRUE) * res
}

# obtiene kmeans; necesita la libreria cluster
# library(cluster)
# input:
#       r_times: vector de tiempos de respuesta
# output: data.frame con óptimo número de clusters (opt_k),
#         start, stop, spread y middle obtenido con kmeans (no es muy confiable)
bp_km <- function(r_times){

  # get a 2x2 df with #resp per second
  res <- 1
  x <- seq(1,180,res)
  r_times <- get_bins(r_times,1,180,1)
  resp_sec <- r_times %>%
  {
    bins <- .
    rate <- f_table(bins,1,180,res) # f_table is a custom fn
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
