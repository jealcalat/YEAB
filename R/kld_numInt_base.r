

kld_np = function(x,y,...) {
  integrand = function(t,x,y){
    f.x <- fitted(npudens(tdat=x,edat=t,bws=bw.SJ(x),...)) + 0.0000001
    f.y <- fitted(npudens(tdat=y,edat=t,bws=bw.SJ(y),...)) + 0.0000001
    tmpRatio = f.x *(log2(f.x) - log2(f.y))
    return(tmpRatio)
  }
  return(integrate(integrand,-Inf,Inf,x = x,y = y, stop.on.error=FALSE)$value)
}


kld_base = function(x,y,...){
  integrand = function(x,y,t){
    f.x =  approx(density(x)$x,density(x)$y,t)$y
    f.y =  approx(density(y)$x,density(y)$y,t)$y
    tmpRatio = f.x *(log2(f.x) - log2(f.y))
    tmpRatio = ifelse(is.infinite(tmpRatio),0,ifelse(is.na(tmpRatio),0,tmpRatio))
    return(tmpRatio)
  }
  return(integrate(integrand,-Inf,Inf,x = x,y = y,stop.on.error=FALSE)$value)
}

set.seed(13)
x = rnorm(100)
y = rnorm(100)
kld_base(x,y)

kld_np(x,y)
# 
# kldr = replicate(50,{
#   x = rnorm(1000)
#   y = rnorm(1000)
#   dkl = kld_np(x,y)
# })
# 
# kld2 = replicate(50,{
#   x = rnorm(1000)
#   y = rnorm(1000)
#   dkl = kld_base(x,y)
# })
# 
# 
# kbar = data.frame(x = kld2,y = kldr)
# 
# boxplot(kbar)
# par(mfrow =c(1,2))
# hist(kldr)
# hist(kld2)

t = seq(min(x),max(x),0.01)
f.x <- fitted(npudens(tdat=x,edat=t,bws=bw.SJ(x)))
f.y <- fitted(npudens(tdat=y,edat=t,bws=bw.SJ(y)))


par(mfrow = c(1,2))
plot(f.x)
lines(f.y)
plot(f.x *(log2(f.x) - log2(f.y)),col = 'red')

