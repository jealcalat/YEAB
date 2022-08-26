# Implements curvature index by integration and the Fry index

curv_index_int = function(cr, t, h) { # h is height
  # Curvature index with numerical integration
  # Get the AUC using numerical integration with cubic splines interpolation

  # total area of a rectangle
  area0 = h * max(cr) / 2
  # integrate needs sfsmisc
  a = min(t)
  b = t[which.max(cr)]
  area1 = integrate.xy(t, cr, a, b)
  index = (area0 - area1) / area0
  return(index)
}

curv_index_fry = function(cr, t, DT, b) {
  # Curvature index using Fry method 
  # Size of subintervals
  size = round(b/DT)
  # Subintervals generated evenly, from 0 to max(t)
  intervals = seq(0, max(t), size)
  # Responses at each subinterval
  resps = cr[t %in% intervals]
  indexFry = (3 * resps[4] - 2 * (resps[1] + resps[2] + resps[3])) / (4 * resps[4])
  return(indexFry)

}



## test
# 


# fry = curv_index_fry(cr=cumsum(df$y), t=df$x, 5, 99)
# numint = curv_index_int(cr=cumsum(df$y), t=df$x, 99)
# 
# 
# plot(t, cr, type = 's')
# points(intervals[-1], resps, col = 'red', pch = 16)
# 
# # segments(x0=intervals[-length(intervals)],
# #          x1=intervals[-1],
# #          y0=c(0,resps[-length(resps)]),
# #          y1=resps)
# # segments(x0=intervals[-1],
# #          x1=intervals[-1],
# #          y0=rep(0,length(resps)),
# #          y1=resps)
# segments(x0=0, y0=0, 
#          x1=t[which.max(cr)],y1=max(cr), lty=2, lwd=2.5)
# segments(x0=t[which.max(cr)], y0=0, 
#          x1=t[which.max(cr)],y1=max(cr), lty=2, lwd=2.5)
# segments(x0=0, y0=0, x1=t[which.max(cr)],y1=0, lty=2, lwd=2.5)
# 
# segments(x0=b * numint,y0=0,x1=b,y1=max(cr), col = 'red')
# segments(x0=b * fry,y0=0,x1=b,y1=max(cr), col = 'blue')

