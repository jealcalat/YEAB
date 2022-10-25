# stat_boot_dopar_byx.R
# Using foreach and dopar, the function groups by x (e.g., bins or session)
# and computes a 'stat', specified by argument FUN (by default the mean), and
# the 95 % CI using boostrapping (resamples in R).
#
# It returns the mean (default 'y'), the lower (lCI) and upper (uCI) confidence
# intervals, as well as the grouping factor (default 'y').

library(foreach)
library(doParallel)
ncores <- parallel::detectCores()
registerDoParallel(cores = ncores - 1)

stat_boot_dopar_byx <- function(x, y,
                                R, FUN = "mean",
                                cnames = c("x", "y")) {
  f <- match.fun(FUN)

  r <- foreach(
    i = 1:R,
    .combine = rbind,
    .packages = "magrittr"
  ) %dopar% {
    idx <- sample(length(x), length(x), replace = T)
    Rx <- x[idx]
    Ry <- y[idx]

    data.frame(Rx, Ry) %>%
      dplyr::group_by(Rx) %>%
      dplyr::summarise(boot_stat = f(Ry))
  }

  r <- r %>%
    group_by(Rx) %>%
    summarise(
      m = f(boot_stat),
      lCI = quantile(boot_stat, 0.025),
      uCI = quantile(boot_stat, 0.975)
    )
  colnames(r)[c(1, 2)] <- cnames
  r
}
