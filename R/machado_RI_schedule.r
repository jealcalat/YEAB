# machado intervals
Tt = 90
p = 0.5

trials = 1000
xtrials = numeric(100)
intervals = numeric(100)

for (ti in 1:trials) {
  u = runif(1, 0, 1)
  t = u * 2* Tt
  r = runif(1, 0, 1)
  xtrials[ti] = ifelse(r < p * (2 * Tt - t)/Tt, 1, 0)
  intervals[ti] = t
}

rf_intervals = intervals[as.logical(xtrials)]
x11()
hist(intervals[which(xtrials == 1)], freq = F)

points(x = c(mean(rf_intervals), 2 * (T/3)), y = c(-0.0002,-0.0002),
       col = c(1,2), pch = c(2,2))
box()
segments(x0 = 0,
         x1 = 2 * Tt,
         y0 = 1/Tt,
         y1 = 0)

sum(xtrials) / trials

x11()
hist((p/Tt) * (2 * Tt - intervals))

x11()
plot(cumsum((1/Tt) * (1 - intervals / (2* Tt))))
