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
