## Doesn't work for res < 1
# get_bins <- function(x, x.min, x.max, resolution){
#   bins <- x %>% # lambda expression
#   {
#     t <- . # basically asign x to t to be used as a placeholder variable
#     t <- cut(t,(x.min:x.max)*resolution) # find intervals between xmin and xmax
#     t <- ifelse(is.na(t),0,t) # return the levels of the intervals
#     t %>% as.integer() # convert to integers
#   }
#   bins # same as return(x), but more efficient
# }

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

## NOT RUN
## Example of use:
# x <- 1:20
# get_bins(x, 0, 20, 5)
## Returns
## [1]  5  5  5  5  5 10 10 10 10 10 15 15 15 15 15 20 20 20 20 20
#
# set.seed(10)
# x <- runif(20, 0, 10)
# get_bins(x, 0, 10, 0.5)
# # Returns
## 1] 5.5 3.5 4.5 7.0 1.0 2.5 3.0 3.0 6.5 4.5 7.0 6.0 1.5 6.0 4.0 4.5 1.0 3.0 4.0 8.5