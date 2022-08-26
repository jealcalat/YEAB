# Convert colors to RGBA
addalpha <- function(cols, alpha) {
  rgb(t(col2rgb(cols)/255), 
      alpha = alpha)
  
}
