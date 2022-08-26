# Single breakpoint algorithm, as the one used in Guilhardi & Church 2004.
# Works in fixed interval trials.

bp_fi <- function(r_times) {
  
  if (max(r_times) < 60) {
    trial_duration = 60
  } else {
    trial_duration = max(r_times)
  }
  
  nr = length(r_times)
  r = nr / trial_duration
  A = numeric(nr - 1)
  
  for (j in 1:(nr - 1)) {
    t1 = r_times[j]
    t2 = trial_duration - t1
    r1 = sum(r_times <= t1) / t1
    r2 = sum(r_times > t2)  / t2
    A[j] = sum(t1 * abs(r1 - r), t2 * abs(r2 - r), na.rm = T)
  }
  
  argmax_A = which.max(A)
  bp = r_times[argmax_A]
  r1 = sum(r_times <= bp) / bp
  r2 = sum(r_times > bp)  / (trial_duration - bp)
  
  data.frame(bp = bp, r1 = r1, r2 = r2)
}
