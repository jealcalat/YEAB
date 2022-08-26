# Depends on npudens fromt he package np
# This is a modification of a solution given in vignette()
kld = function(x,y,...) {
  integrand = function(t,x,y){
    # Compute the probability by nonparametric kernel smoothing
    # - For bw.SJ see ?bw.SJ which computes the bandwidth
    #   for the kernel... which is approx the same as the variance
    #   in the gaussian function.
    # The argument 'edat=t' is for evaluate at 't'. Ie, the integral
    # will integrate over 't'.
    f.x <- fitted(npudens(tdat=x,edat=t,bws=bw.SJ(x),...))  + 0.0000001
    f.y <- fitted(npudens(tdat=y,edat=t,bws=bw.SJ(y),...))  + 0.0000001
    # get the funtion to be integrated
    tmpRatio = f.x *(log2(f.x) - log2(f.y))
    
    # this is the h
    # tmpRatio = 0.5*(sqrt(f.x)-sqrt(f.y))**2
    
  # tmpRatio = ifelse(is.infinite(tmpRatio),0,ifelse(is.nan(tmpRatio),0,tmpRatio))
    return(tmpRatio)
  }
  # Integrate in the domain of reals, i.e., [-Inf, Inf],
  # return the integral value. 
  return(integrate(integrand,-Inf,Inf,x = x,y = y, stop.on.error=FALSE)$value)
}

x = rnorm(100,0,1)
y = rnorm(100,0,1)

kld(x,y)

sds = c(1,2,5,10,15)

kls = numeric(5)

for(i in 1:5){
  x = rnorm(100,0,1)
  y = rnorm(100,sds[i],1)
  kls[i] = kld(x,y) 
}

plot(sds,kls)
