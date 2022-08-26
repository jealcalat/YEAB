

hyp = function(k, d){
  1 / (1 + k * d)
}

d = 1:100
k = 0.2
vh = hyp(k,d) + rnorm(length(d), 0, 0.1)

plot(d,vh)

loess_hy = loess(vh ~ d)
splines_hy = spline(d,vh)
plot(d,vh)
lines(fitted(loess_hy))
lines(x=splines_hy$x, y=splines_hy$y)
#lines(dvh)

deriv_spline = splinefun(d,vh)
dvh = deriv_spline(d,deriv = 1)

integrate(deriv_spline, 0, 100)
integrate(function(x) {1 / (1 + k * x)}, 0, 100)

plot(dvh, type = 'l')
