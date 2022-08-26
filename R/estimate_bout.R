### bout parameters ------------------------------------------------------------#
# using maximum likelihood via VGAM package
# see https://rdrr.io/cran/VGAM/man/mix2exp.html
# AUTHOR: Hiroshi Matsui
estimate_bout = function(irt){
  # test
  # dat = c(rexp(1000, rate=1), rexp(500, rate=50))
  # dat = sort(dat)
  # fit = VGAM::vglm(dat ~ 1, VGAM::mix2exp, trace = TRUE)
  
  fit = VGAM::vglm(irt ~ 1, VGAM::mix2exp, trace = TRUE)
  param = VGAM::Coef(fit)
  
  params = data.frame(
    bout_length = 1/param[1],
    bout_initiation = 1/param[2],
    bout_within = 1/param[3])
  return(params)
}