df = data.frame (x = 1:180,
                 y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                       0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,2,2,4,2,2,3,2,1,2,0,1,0,1,4,
                       0,1,2,3,1,1,1,0,2,0,3,2,1,1,1,1,5,4,2,1,0,2,1,1,2,0,0,2,2,
                       1,1,1,0,0,0,0,2,3,0,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0))

tsdf = ts(df$y)
plot(tsdf)

library(strucchange)
library(changepoint)
library(changepoint.np)
bps = breakpoints(tsdf ~ 1, breaks = 6)
cps = cpt.meanvar(tsdf, test.stat = 'Poisson', 
                  penalty = 'Manual', pen.value = '5*log(n)',
                  method = 'BinSeg', minseglen = 5)
cps2 = cpt.np(tsdf, penalty = 'SIC')


res1 = (resid(bps) %*% resid(bps))

fun_res_cp = function(cps,df) {
  
  if (class(cps) == 'ocp') {
    
    pts = ocpd_timing$changepoint_lists$maxCPs[[1]]
    
  } else {
    pts = cpts(cps)
  }
  
  y_obs = df$y
  t = df$x
  
  means = c()
  
  if (tail(pts, 1) < max(t)) {
    pts = c(pts, max(t))
  }
  ncp = length(pts)
  for (i in 1:ncp) {
    
    tslice = which(t <= pts[i])
    r = y_obs[tslice]
    mean_i = rep(mean(r), length(r))
    means = c(means, mean_i)
    y_obs = y_obs[-tslice]
    t = t[-tslice]
  }
  res = (means - df$y)
  res = res %*% res
  list(means, res)
}

# Adams and MaCkay
# install.packages('ocp')
library(ocp)
# the true changepoint locations including the first and last point
truecps<- c(1, 25, 65, 121)
#simulate the data
set.seed(1)
uvg <- c(rnorm(n=diff(truecps)[1], mean=5, sd=1), 
         rnorm(n=diff(truecps)[2], mean=20, sd=2),
         rnorm(n=diff(truecps)[3], mean=10, sd=3))
ocpd_output <- onlineCPD(uvg)


par(mgp = c(1.5,0.5,0),
    mar = c(2.5,2.5,1.5,2))
plot(uvg, type = 'p', pch = 16, col = 'blue',
     axes = F,
     ylab = 'Rate',
     xlab = 'Time',
     panel.first = {
       grid()
       axis(1, tck = -0.015)
       axis(2, tck = -0.015, las = 2)
     })
abline(v = ocpd_output$changepoint_lists$maxCPs[[1]][2:3],
       col = 'red', lty = 2)
# with my data

## Always drop the first cp, which it seems to be always the first 1
ocpd_timing = onlineCPD(df$y, probModel = 'poisson')

plot(ocpd_timing)


means = fun_res_cp(cps, df)
means_obcp = fun_res_cp(ocpd_timing,df)
plot(df)
lines(fitted(bps), type = 's')
lines(means[[1]], col = 'red', type = 's')
lines(means_obcp[[1]], col = 'blue', type = 's')
dr = density(df$x[df$y>0], adjust = 0.8)
scalar = 5/max(dr$y)
lines(dr$x, dr$y * scalar)



plot(cumsum(df$y), type = 's')
abline(v = bps$breakpoints)
abline(v = cpts(cps2), col = 'red')
abline(v = ocpd_timing$changepoint_lists$maxCPs[[1]])


