
pretty_hist <- function(x, # vector
                        n_bins = 30,
                        alpha = 0.5,
                        lwd = 1,
                        bar_fill = "orange",
                        line_col = "black"){
  
  r <- range(x)
  
  by <- diff(r/(n_bins))
  
  breaks <- seq(from = r[1],
                to = r[2],
                by = by)
  
  f <- 0
  
  for(i in 1:(length(breaks)-1)){
    
    f[i] <- sum(x >= breaks[i] & x < breaks[i + 1])
    
  }
  
  f[n_bins] <- sum(x > breaks[n_bins])
  
  f[n_bins + 1] <- f[n_bins]
 
  freq <- data.frame(x = c(min(breaks),
                           breaks,
                           max(breaks)),
                     y = c(0,f,0))
  
  x <- as.data.frame(x)
  
  ggplot(freq) +
    geom_histogram(data = x,
                   fill = bar_fill,
                   alpha = alpha,
                   mapping = aes(x),
                   breaks = breaks) +
    geom_step(aes(x,y),
              size = lwd,
              color = line_col) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(size = lwd, colour = "black"),
          panel.background = element_blank())
}
# 
# d <- rexp(10000)
# 
pretty_hist(rnorm(1000, 10, 10),
            line_col = "black",
            bar_fill = palette()[2],
            lwd = 0.5,
            n_bins = 10)
# 
