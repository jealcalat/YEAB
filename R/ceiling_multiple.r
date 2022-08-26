# Find the next number greater than 'x' which is a multiple of 'multiple'
# that is, (x + e) %% multiple = 0, on which e > 0

ceiling_multiple = function(x, multiple) {
  m = x %% multiple
  residual = multiple - m
  x + residual
}
