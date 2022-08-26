kde_epa <- function(xr, h, from, to, n=512){
 
  xp <- function(x) (3/4)*(1 - x^2) * (abs(x) < 1)
  
  xrange <- seq(from, to, length.out = n)

  dist_norm <- function(xr = xr, xeval, h){
    di <- (xeval - xr)/h
    kv <- sapply(di, xp)
    ks <- sum(kv)
    ks /(length(xr) * h)
  }
  
  kder <- sapply(xrange, function(xx){ dist_norm(xr, xeval = xx, h = h) })
  data.frame(
    x = xrange,
    y = kder
  )
}

rx <- runif(100)

hist(rx, freq = FALSE)
lines(density(rx, kernel = 'e', from = 0, to=max(rx)))
lines(kde_epa(xr = rx,h = bw.nrd0(rx), from = 0, to = max(rx), n = 1000), col = 2)
