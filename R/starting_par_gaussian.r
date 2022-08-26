gausslin.start <- function(x,y) {
  
  theilreg <- function(x,y){
    yy <- outer(y, y, "-")
    xx <- outer(x, x, "-")
    z  <- yy / xx
    slope     <- median(z[lower.tri(z)])
    intercept <- median(y - slope * (x - mean(x)))
    cbind(intercept=intercept,slope=slope)
  }
  
  tr <- theilreg(x,y1)
  abline(tr,col=4)
  Ds = tr[2]
  Es = tr[1]
  yf  <- y1-Ds*x-Es
  yfl <- loess(yf~x,span=.5)
  
  # assumes there are enough points that the maximum there is 'close enough' to 
  #  the true maximum
  
  yflf   <- yfl$fitted    
  locmax <- yflf==max(yflf)
  Bs     <- x[locmax]
  As     <- yflf[locmax]
  
  qs     <- yflf>.6*As
  ys     <- yfl$fitted[qs]
  xs     <- x[qs]-Bs
  lf     <- lm(ys~xs+I(xs^2))
  bets   <- lf$coefficients
  Bso    <- Bs
  Bs     <-  Bso-bets[2]/bets[3]/2
  Cs     <- sqrt(-1/bets[3])
  ystart <- As*exp(-((x-Bs)/Cs)^2)+Ds*x+Es
  
  y1a <- y1-As*exp(-((x-Bs)/Cs)^2)
  tr  <- theilreg(x,y1a)
  Ds  <- tr[2]
  Es  <- tr[1]
  res <- data.frame(As=As, Bs=Bs, Cs=Cs, Ds=Ds, Es=Es)
  res
}

# y1 <- datos_test$tasa_r+5 + datos_test$bins*.09 # This is the data
# xo <- order(datos_test$bins)
# 
# starts <- gausslin.start(x=datos_test$bins, y = datos_test$tasa_r)
# ystart <- with(starts, As*exp(-((datos_test$bins-Bs)/Cs)^2)+Ds*datos_test$bins+Es)
# plot(datos_test$bins,datos_test$tasa_r)
# 
# lines(datos_test$bins,ystart,col=2)
# 
# starts
# 
# formula <- tasa_r ~ a * exp(-0.5 * ((bins - t0) / b)**2) + c * (bins - t0) + d
# 
# 
# fit.nlxb <- nlxb(formula , data = datos_test,
#                  start = c(a = 44, d = 12, t0 = 20, b = 10, c = 1),
#                  lower = c(a = 1, d = 12, t0 = 0, b = 1, c = -0.5),
#                  upper = c(a = 100, d = 12, t0 = 50, b = 10, c = 5))
# coefs <- fit.nlxb$coefficients
# 
# ystart <- coefs["a"] * exp(-0.5 * ((datos_test$bins - coefs["t0"])/coefs["b"])**2) + 
#   coefs["c"] * (datos_test$bins - coefs["t0"]) + coefs["d"]
