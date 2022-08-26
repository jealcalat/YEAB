
rn = c(rnorm(10, 15, sd = 1), 
       rnorm(10, 20, sd = 1), # Cambio en media
       rnorm(10, 17, sd = 4)) # Cambio en varianza

par(mfrow = c(1,3))

hist(rn[1:100], xlim = c(5, 30))
hist(rn[101:200], xlim = c(5, 30))
hist(rn[201:300], xlim = c(5, 30))

plot(rn, type = 'l', ylim = c(0,30))
lines(c(rep(15,10),rep(17,10),rep(17,10)),col = 4,lwd=2, type = 's')
abline(v = c(100,200),col = 2)

library(changepoint)

# Argumentos por default
cp_mean_default = cpt.mean(rn)
cp_var_default = cpt.var(rn)

plot(cp_mean_default, ylim = c(0,30), lty = 2)
points(x = c(100,200),
       y = rn[c(100,200)],
       col = 6, pch = '^', cex = 2.5)
abline(v = cp_var_default@cpts, col = 4, lty = 2, lwd = 2)
# cpt.mean detecta bien el cambio en media correctamente

## Otros métodos (no default) y otro penalty

cp_mean_BS = cpt.mean(rn, test.stat = 'Normal', method = 'BinSeg', penalty = 'BIC', Q =1)
cp_var_BS = cpt.var(rn, test.stat = 'Normal', method = 'BinSeg', penalty = 'BIC')
plot(cp_mean_BS,col = 'grey40',type = 'l', ylim = c(0,30), lty = 2)
points(x = c(100,200),
       y = rn[c(100,200)],
       col = 6, pch = '^', cex = 2.5)
abline(v = cp_var_BS@cpts, col = 4, lty = 2)

cpm = cpt.mean(rn, method = 'PELT')

plot(cpm)

## EN este caso, BS depende de Q. Si se especifican muchos, tratará de encontrarlos.
## Si se specifica 1, no intentara encontrarlos. Probablemente otro método sería más
## apropiado cuando no se conocen a priori los posibles cambios. AMOC?

## GAM

library(mgcv)
tmpf <- tempfile()
download.file("https://gist.githubusercontent.com/gavinsimpson/ca18c9c789ef5237dbc6/raw/295fc5cf7366c831ab166efaee42093a80622fa8/derivSimulCI.R",
              tmpf, method = "wget")
source(tmpf)




# tryon

z = numeric((length(rn) - 8))
p = numeric((length(rn) - 8))
for(i in 9:length(rn)){
 tr = tryon(rn[(i - 8):i])
 z[(i-8)] = tr$z
 p[(i-8)] = tr$p.value
}
z
p
