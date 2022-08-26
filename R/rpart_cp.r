df = data.frame (x = 1:180,
                 y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                       0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,2,2,4,2,2,3,2,1,2,0,1,0,1,4,
                       0,1,2,3,1,1,1,0,2,0,3,2,1,1,1,1,5,4,2,1,0,2,1,1,2,0,0,2,2,
                       1,1,1,0,0,0,0,2,3,0,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0))

tree <- rpart(y ~ x, data=df, method = 'poisson')

plot_tree <- function(tree, x, y) {
  s <- seq(0, 180, by=0.5)
  plot(x, y)
  lines(s, predict(tree, data.frame(x=s)))
}

plot_tree(tree, df$x, df$y)
abline(v = tree$splits[,4])

df = data.frame(x = 1:180,
                 y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                       0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,2,2,4,2,2,3,2,1,2,0,1,0,1,4,
                       0,1,2,3,1,1,1,0,2,0,3,2,1,1,1,1,5,4,2,1,0,2,1,1,2,0,0,2,2,
                       1,1,1,0,0,0,0,2,3,0,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,2,2,3,2,1,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0))
plot(df2)

tree2 = rpart(y ~ x, data=df2, method = 'poisson')
plot_tree(tree2, df2$x, df2$y)
abline(v = tree2$splits[,4])
