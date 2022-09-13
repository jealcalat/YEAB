#NECESITA INSTALARSE LA FUNCION ZOO
#Este análisis es del articulo sex differences de balci 2019
#install.packages("zoo")

# input: 
# tasa_norm: vector numérico con la tasa de respuesta normalizada
# bines: vector numérico con el tiempo en bines 

# output: start, stop, spread y bin de máxima tasa (peak time)

balci2010 = function(tasa_norm, bines) {
  
  mov_av = rollapply(tasa_norm,
                     width = 3, 
                     FUN = mean, 
                     align = "center", 
                     partial = TRUE)
  
  max_rnorm = max(mov_av)
  binmax = bines[mov_av == max_rnorm]
  
  start = bines[bines < binmax & mov_av >= 0.7][1]
  stop = bines[bines > binmax & mov_av < 0.7][1]
  spread = stop - start
  
  c(start, stop, spread,binmax)
}
# plot(mov_av, type = 'l', ylim = c(0,1))
# lines(tasa_norm, type = 'l', col = 'red')
