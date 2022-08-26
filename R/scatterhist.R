# Makes a scatter plot with marginal histograms

scatterhist <- function(x, y, 
                        xlab="D", 
                        ylab="x",
                        border = 'grey',...){
  zones <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths = c(4/5,1/5), 
         heights = c(1/5,4/5))
  xhist <-  hist(x, plot = FALSE)
  yhist <-  hist(y, plot = FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,0.2,0.2))
  plot(x,y,pch = 21,...)
  abline(v = 0,h = 0,lty = 2)
  par(mar=c(0,3,0.2,0.2))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0,border = border)
  par(mar=c(3,0,0.2,0.2))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, 
          horiz=TRUE,border = border)
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}
## How to use
# set.seed(123)
# x <- rnorm(100); y <- rnorm(100)
# scatterhist(x,y,bg = 'red')
# 
