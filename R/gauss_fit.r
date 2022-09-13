#' Fit a gaussian plus a ramp component for the performance in a peak procedure task
#'
#' @param respuestas numeric, vector of response or response rate
#' @param t numeric, time bins
#' @param par a list of parameters for the gaussian + linear;
#'   see Buhusi, C. V., Perera, D., & Meck, W. H. (2005) for an explanation
#' @param max.iter numeric, max number of iterations
#'
#' @return a numeric vector of coefficients
#' @export
#'
#' @details This algorithm uses the nonlinear least squares nls.lm from the minpack.lm package
#'
#' @examples
gaussian_fit <- function(respuestas,t,
                         par=list(a=0.1,
                                  d=0.1,
                                  t0=18, # valor próximo a IF
                                  b=10,
                                  c=1),
                         max.iter=500){
  # Ajuste de funcion gaussiana con rampa.
  # Ver Buhusi, C. V., Perera, D., & Meck, W. H. (2005).
  # Memory for timing visual and auditory signals in albino and pigmented rats.
  ## ===========================================================================
  ## Nota: Usa nls.lm con el algoritmo de Levenberg–Marquardt, por lo que es
  ## necesario instalar minpack.lm:
  ## install.packages("minpack.lm")
  ## ===========================================================================
  ## Argumentos:
  ##  respuestas: vector de respuestas por bin
  ##  t: vector de bines correspondientes a las respuestas
  ##  par: parámetros de la función gaussiana en Buhusi et al. Ajustar si
  ##  algoritmo no converge.
  ##  max.iter: maximo numero de iteraciones para forzar convergencia.
  ##            en nls.lm por defecto tiene 50, aquí se usan 500.

  ## Output:
  ##  Coeficientes de ajuste de la regresión

  # Parte derecha de la funcion gaussiana

  g_plus_lin <- function(par,t) {

    par$a*exp(-0.5*((t - par$t0)/par$b)**2) + par$c*(t - par$t0)+par$d
  }

  # Funcion residual de la forma: observado - predicho
  res_fun <- function(respuestas,par,t){
    respuestas - g_plus_lin(par,t)
  }
  # Ajuste usando Levenberg–Marquardt
  nlm_F <- nls.lm(par=par,
                  fn=res_fun,
                  y=respuestas,
                  t=t,
                  control = nls.lm.control(maxiter = max.iter))
  coef(nlm_F)
}
