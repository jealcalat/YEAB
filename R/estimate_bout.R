#' Biexponential model for the estimation of within and between bouts
#'
#' @param irt numeric, the interresponse times
#'
#' @return numeric vector of the biexponential model parameters \eqn{L=1/\theta}, the bout length,
#' \eqn{W = 1/w}, the within bout rate, and \eqn{B = 1/b} the bout initiation rate, where
#' \eqn{\theta, w, b} are the proportion of responses that are bouts, the mean within bout IRT, and
#' the mean between-bout IRT.
#' @export
#'
#' @details Implements the finite mixtures of two exponentials
#' \eqn{p(IRT < \tau) = \theta w e^{-w IRT} + (1-\theta)b e^{-b IRT}}
#'
#'
#' @examples
estimate_bout <- function(irt) {
  fit <- VGAM::vglm(irt ~ 1, VGAM::mix2exp, trace = TRUE)
  param <- VGAM::Coef(fit)
  params <- data.frame(
    bout_length = 1 / param[1],
    bout_initiation = 1 / param[2],
    bout_within = 1 / param[3]
  )
  return(params)
}
