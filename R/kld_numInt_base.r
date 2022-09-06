

kld_np <- function(x, y, ...) {
  integrand <- function(t, x, y) {
    f.x <- fitted(npudens(tdat = x, edat = t, bws = bw.SJ(x), ...)) + 0.0000001
    f.y <- fitted(npudens(tdat = y, edat = t, bws = bw.SJ(y), ...)) + 0.0000001
    tmpRatio <- f.x * (log2(f.x) - log2(f.y))
    return(tmpRatio)
  }
  return(integrate(integrand, -Inf, Inf, x = x, y = y, stop.on.error = FALSE)$value)
}


kld_base <- function(x, y, ...) {
  integrand <- function(x, y, t) {
    f.x <- approx(density(x)$x, density(x)$y, t)$y
    f.y <- approx(density(y)$x, density(y)$y, t)$y
    tmpRatio <- f.x * (log2(f.x) - log2(f.y))
    tmpRatio <- ifelse(is.infinite(tmpRatio), 0, ifelse(is.na(tmpRatio), 0, tmpRatio))
    return(tmpRatio)
  }
  return(integrate(integrand, -Inf, Inf, x = x, y = y, stop.on.error = FALSE)$value)
}
