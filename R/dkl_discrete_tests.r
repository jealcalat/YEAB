
x2den2prob.mass <- function(x, n) {
  d <- density(x, n = n)
  den.x <- d$x
  den.y <- d$y
  # interval of the prob. density
  delta.x <- den.x[2L] - den.x[1L] 
  # normalizing constant
  K <- sum(den.y) * delta.x 
  
  ## Because density = probability mass / delta.x,
  ## probability mass = density * delta.x 
  ## but because this may not sum to 1, K is computed for make
  ## prob.mass a true probability
  prob.mass <- den.y * delta.x / K
  
  # return prob.mass
  prob.mass
}

## Example computing KL-divergence
## div = P(X) * log(P(X) / Q(X))
## dkl = sum(div)
# p <- rnorm(100, 10, 1)
# q <- rnorm(100, 11, 1)
# 
# p <- x2den2prob.mass(p, 100)
# q <- x2den2prob.mass(q, 100)
# 
# div <- p * (log(p) - log(q))
# sum(div)
# 
dkl_discrete <- function(p, q, n){
  p <- x2den2prob.mass(p, n)
  q <- x2den2prob.mass(q, n)
  div <- p * (log(p/q, base = 2))
  div <- div[!is.infinite(div)]
  sum(div)
}
# 
# ## Let's do some test for 100 q with different mean
# means.vec <- seq(0.1, 20, 0.5)
# p <- rnorm(1000, 0, 1)
# 
# test.dkl <- c()
# 
# for (x in means.vec) {
#   q <- rexp(1000, rate = 1/x)
#   rep.x <- replicate(100, {
#     dkl_discrete(p, q, 512)
#   }) 
#   test.dkl <- c(test.dkl, mean(rep.x))
# }
# 
# plot(means.vec, test.dkl, col = 2, type = 'o')
