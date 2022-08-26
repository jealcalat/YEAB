# important custom functions ----

source("~/Dropbox/R_functions_mis/box_dot_plot.r")
source("~/Dropbox/R_functions_mis/addalpha.R")
source("~/Dropbox/R_functions_mis/hist_overlap.R")

# plus concatenator


"+" <- function(x,y) {
  if (is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}
