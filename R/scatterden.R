scatterden <- function(x, y, 
                        xlab="D", 
                        ylab="x",
                        border = 'grey',op,...){
  zones <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths = c(4/5,1/5), 
         heights = c(1/5,4/5))
  dx <-  density(x)
  dy <-  density(y)
  top <- max(c(dx$y, dy$y))
  par(op,mar=c(3,3,0.2,0.2))
  plot(x,y,pch = 21)
  abline(v = 0,h = 0,lty = 2)
  par(op,mar=c(0,3,1,0.2))
  plot(dx, axes=FALSE,type = 'l',main = '')
  par(op,mar = c(3,0.21,0,1))
  plot(x = dy$y,y = dy$x, axes=FALSE,type = 'l',main = '')
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}
scatterden(x,y,op=par)
