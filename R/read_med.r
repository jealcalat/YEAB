# Paths to directories have the form of windows paths, like

#
read_med <- function(fname, # Name of the MED file to read;
                     # can include the path directory
                     path_save, # Path to folder to save csv files;
                     # e.g. /Dropbox/exp1/Phase_1/
                     col_r = "C:", # Variable of MED to read
                     out = T) {
  options(stringsAsFactors = FALSE)
  dfx <- read.table(fname,
    skip = 3, na.strings = "NA", fill = T,
    col.names = paste0("V", seq_len(6))
  )

  a <- which((dfx$V1 == "0:"))
  col_pos <- which(dfx$V1 == col_r) + 1
  idx1 <- a[a == col_pos]
  idx2 <- a[which(a == idx1) + 1]
  if (length(idx2) == 0) {
    idx2 <- nrow(dfx)
  }
  varY <- dfx[idx1:idx2, ]
  varY[1] <- NULL # Because 1st column are just row names

  # Check vector class and change to numeric
  for (j in 1:ncol(varY)) {
    if (is.factor(varY[[j]])) {
      varY[, j] <- as.numeric(as.character(varY[[j]]))
    }
    if (is.character(varY[[j]])) {
      varY[, j] <- as.numeric(varY[[j]])
    }
  }
  # Stack matrix in two vectors
  varY <- na.omit(stack(varY))
  # Erase second vector
  varY[2] <- NULL
  # Sort by time the time.event vector
  varY <- data.frame(val = varY$values)
  varY <- varY[varY$val > 10, ]
  var_tmp <- do.call(rbind, strsplit(as.character(varY), "\\."))
  var_tmp <- as.data.frame(var_tmp)

  if (ncol(var_tmp) > 1) {
    colnames(var_tmp) <- c("tiempo", "evento")
  }


  for (c in 1:ncol(var_tmp)) {
    var_tmp[, c] <- as.numeric(var_tmp[, c])
  }

  fname <- unlist(strsplit(f, " "))[2]
  fname <- unlist(strsplit(fname, "\\."))[1]

  filesave <- sprintf("%s%s.csv", path_save, fname)
  write.csv(var_tmp,
    file = filesave,
    row.names = FALSE
  )
  if (out) {
    return(var_tmp)
  }
}

path_tidy <- "/run/media/mrrobot/D4A6-7BC9/tidy/"

filesn <- list.files(pattern = "^IF")

for (f in filesn) {
  read_med(f, path_tidy)
}
