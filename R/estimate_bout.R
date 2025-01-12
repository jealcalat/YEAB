#' @title Biexponential Model
#'
#' @description Implements a simpler biexponential model without the refractory period parameter, \eqn{\delta}.
#'
#' The simpler model is defined as:
#' \deqn{p(IRT = \tau) = p w e^{-w \tau} + (1 - p) b e^{-b \tau}}
#' where \eqn{w} and \eqn{b} represent the within- and between-bout response rates, and \eqn{p} is the proportion of responses in bouts.
#'
#' @param irt A numeric vector representing inter-response times.
#'
#' @return A data frame with estimated parameters \eqn{w} (proportion of responses in bouts), \eqn{l0} (within-bout mean IRT),
#' and \eqn{l1} (between-bout mean IRT).
#'
#' @examples
#' set.seed(43)
#' l1 <- 1 / 0.5
#' l2 <- 1 / 0.1
#' p <- 0.4
#' n <- 200
#' irt <- c(rexp(round(n * p), l1), rexp(round(n * (1 - p)), l2))
#' biexponential(irt)
#'
biexponential <- function(irt) {
  optimize_biexponential(irt)
}

#' @title Optimization Function for the Biexponential Model
#' @description Optimizes the log-likelihood function to estimate biexponential model parameters based on observed inter-response times.
#' @param irt A numeric vector of inter-response times.
#' @return A named vector of optimized parameters for the biexponential model.

optimize_biexponential <- function(irt) {
  # Initial parameter guesses
  initial_params <- c(w = 0, l0 = log(1), l1 = log(1))

  # Use optim to minimize the negative log-likelihood
  optim_res <- optim(
    par = initial_params,
    fn = biexponential_log_likelihood,
    irt = irt,
    method = "BFGS"
  )

  # Transform optimized parameters back to constrained space
  w <- 1 / (1 + exp(-optim_res$par[1])) # Proportion of responses in bouts
  l0 <- exp(optim_res$par[2]) # Within-bout mean IRT
  l1 <- exp(optim_res$par[3]) # Between-bout mean IRT
  data.frame(w = w, l0 = l0, l1 = l1)
}

#' @title Log-Likelihood Function for Biexponential Model
#'
#' @description Calculates the negative log-likelihood for the simpler biexponential model, which does not include the
#' refractory period parameter, \eqn{\delta}.
#'
#' @param params A numeric vector of initial parameter estimates for optimization.
#' @param irt A numeric vector representing inter-response times.
#'
#' @details This function computes the negative log-likelihood based on biexponential functions
#' for the simpler biexponential model, adjusting parameters using transformations to meet constraints.
#'
#' @return Negative log-likelihood value used for parameter estimation.
#'
#' @rdname optimize_biexponential
biexponential_log_likelihood <- function(params, irt) {
  # Transform parameters to ensure they are within valid ranges
  w <- 1 / (1 + exp(-params[1])) # Constrain `w` between 0 and 1
  l0 <- exp(params[2]) # Ensure `l0` is positive
  l1 <- exp(params[3]) # Ensure `l1` is positive
  likelihood <- w * dexp(irt, rate = 1 / l0) + (1 - w) * dexp(irt, rate = 1 / l1)

  # Return negative log-likelihood
  -sum(log(likelihood + 1e-9)) # Small constant added to avoid log(0)
}

#' @title Biexponential Refractory Model (BERM)
#'
#' @description Implements the biexponential refractory model (BERM) using maximum likelihood estimation
#' to fit parameters for inter-response times (IRTs) within and between bouts.
#'
#' The model is defined as:
#' \deqn{p(IRT = \tau | \tau \ge \delta) = p w e^{-w (\tau - \delta)} + (1 - p) b e^{-b (\tau - \delta)}}
#' where \eqn{w} and \eqn{b} are the rates for within and between bouts, \eqn{p} is the proportion of responses in bouts,
#' and \eqn{\delta} is the refractory period.
#'
#' @param irt A numeric vector representing the inter-response times.
#' @param delta A numeric value for the refractory period.
#'
#' @return A data frame with estimated parameters \eqn{w} (within-bout rate), \eqn{b} (between-bout rate), \eqn{p}
#' (proportion of responses in bouts), and \eqn{\delta} (adjusted refractory period).
#'
#' @examples
#' set.seed(43)
#' l1 <- 1 / 0.5
#' l2 <- 1 / 0.1
#' p <- 0.4
#' n <- 200
#' delta <- 0.03
#' irt <- c(rexp(round(n * p), l1), rexp(round(n * (1 - p)), l2)) + delta
#' optimize_berm(irt)
#'
#' @export
berm <- function(irt, delta) {
  optimize_berm(irt)
}

#' @title Log-Likelihood Function for BERM
#'
#' @description Calculates the negative log-likelihood for the BERM model.
#'
#' @param params A numeric vector of initial parameter estimates for optimization.
#' @param irt A numeric vector representing inter-response times.
#'
#' @details This function computes the negative log-likelihood based on biexponential functions
#' for the BERM model, adjusting parameters using `param_conver` to meet constraints.
#'
#' @return Negative log-likelihood value used for parameter estimation.
#'
#' @rdname berm
berm_log_likelihood <- function(params, irt) {
  min_irt <- min(irt)
  transformed_params <- param_conver(params, min_irt)

  w <- transformed_params["w"]
  l0 <- transformed_params["l0"]
  l1 <- transformed_params["l1"]
  d <- transformed_params["d"]

  likelihood <- w * dexp(irt - d, rate = 1 / l0) + (1 - w) * dexp(irt - d, rate = 1 / l1)
  -sum(log(likelihood + 1e-9))
}

#' @title Mapping Function for Refractory Period Adjustment
#'
#' @description Maps an unconstrained `d_hat` onto the observed minimum inter-response time (`d`), ensuring
#' that it aligns with model constraints.
#'
#' @param d Minimum inter-response time.
#' @param d_hat Transformed parameter to be mapped onto `d`.
#'
#' @return Adjusted refractory period used in likelihood estimation.
#'
#' @rdname berm
map_onto <- function(d, d_hat) {
  d / (1 + exp(-d_hat))
}

#' @title Parameter Transformation Function for BERM
#'
#' @description Converts raw parameters into their constrained forms to enforce model constraints on
#' parameters such as `w`, `l0`, `l1`, and `d`.
#'
#' @param params A numeric vector of raw, unconstrained parameters.
#' @param min_irt Minimum inter-response time for mapping `d`.
#' @param parnames Optional vector of parameter names for labeling.
#'
#' @return A named numeric vector of transformed parameters with constraints applied.
#'
#' @rdname berm
param_conver <- function(params, min_irt, parnames = c("w", "l0", "l1", "d")) {
  w <- 1 / (1 + exp(-params[1])) # Constrain `w` between 0 and 1
  l0 <- exp(params[2]) # Ensure `l0` is positive
  l1 <- exp(params[3]) # Ensure `l1` is positive
  d <- map_onto(d = min_irt, d_hat = params[4]) # Map `d`
  setNames(c(w, l0, l1, d), parnames)
}

#' @title Optimization Function for the BERM Model
#'
#' @description Optimizes the log-likelihood function to estimate BERM model parameters based on
#' observed inter-response times.
#'
#' @param irt A numeric vector of inter-response times.
#'
#' @return A named vector of optimized parameters for the BERM model.
#'
#' @examples
#' set.seed(43)
#' l1 <- 1 / 0.5
#' l2 <- 1 / 0.1
#' p <- 0.4
#' n <- 200
#' delta <- 0.03
#' irt <- c(rexp(round(n * p), l1), rexp(round(n * (1 - p)), l2)) + delta
#' optimize_berm(irt)
#'
#' @rdname berm
optimize_berm <- function(irt) {
  initial_params <- c(w = 0, l0 = log(1), l1 = log(1), d = log(1))
  optim_res <- optim(par = initial_params, fn = berm_log_likelihood, irt = irt)
  param_conver(optim_res$par, min_irt = min(irt))
}
