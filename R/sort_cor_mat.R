sort_cor_mat <- function(cor_mat) {
  row_names <- row.names(cor_mat)
  if (is.null(row_names)) {
    row.names(cor_mat) <- 1:nrow(cor_mat)
  }
  col_names <- colnames(cor_mat)
  if (is.null(col_names)) {
    colnames(cor_mat) <- 1:ncol(cor_mat)
  }

  r_Vec <- as.vector(cor_mat)
  exp_g <- expand.grid(c(1, 2, 3), c(1, 2, 3))
  exp_g <- exp_g[order(exp_g$Var1), ]
  names_r_Vec <- paste(
    exp_g[, 1], 
    exp_g[, 2],
    sep = "-"
  )
  names(r_Vec) <- names_r_Vec
  # sort by absolut value
  sorted_r <- sort(abs(r_Vec), decreasing = TRUE)
  sorted_r <- (sorted_r * sign(r_Vec[names(sorted_r)]))
  sorted_r <- sorted_r[!duplicated(sorted_r)]
  rdf <- data.frame(
    variables = names(sorted_r),
    cor = sorted_r,
    row.names = NULL
  )
  rdf
}
