#' Biexponential model for the estimation of whithin and between-bouts
#'
#' @param irt numeric, the interresponse times
#'
#' @return numeric vector of the biexponential model parameters
#' @export
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
