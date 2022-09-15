# Paths to directories have the form of windows paths.
# See at the end for an example of how to use this function efficiently

## Input:
#   a MED raw file in the form of time.event convention.
## Output:
#   a csv with two columns: time and events

#' Process MED to csv based on standard data structure event.time
#'
#' @param fname chr, the name of a MED file to read; can include the path directory
#' @param save_file logical, Save csv? TRUE or FALSE (default)
#' @param path_save chr, Path to folder to save csv files;
#' @param col_r chr, Variable of MED-raw to read (a event.time variable)
#' @param out logical, output the dataframe processed?
#'
#' @return
#' @export
#'
#' @examples
read_med <- function(fname, # Name of the MED file to read;
                     # can include the path directory
                     save_file = FALSE, # Save csv? TRUE or FALSE (default)
                     path_save = NULL, # Path to folder to save csv files;
                     # e.g. /Dropbox/exp1/Phase_1/
                     col_r = "C:", # Variable of MED-raw to read
                     out = T) { # store in memory the output file?
  # this will return the data.frame in RAM
  # available to work on it immediatly.
  options(stringsAsFactors = FALSE)
  dfx <- read.table(fname,
    skip = 3, na.strings = "NA", fill = T,
    col.names = paste0("V", seq_len(6))
  )

  # Create a numeric vector of the positions where dfx have "0:",
  # wich in MED is the start of an array. This will tell us where are
  # the variables, including our col_r variable ('C:' by default)
  a <- which((dfx$V1 == "0:"))

  # Look where is col_r (where is "C:" by default), and add 1 to that
  # position, because is where MED start to count the array ("0:")
  col_pos <- which(dfx$V1 == col_r) + 1

  # Of the positions stored in "a", which of them is col_pos?
  idx1 <- a[a == col_pos]
  # And which of them is the NEXT position? That is, where col_r ends!
  # idx1 and idx2 will allow us to cut col_r ("C:" by default)
  idx2 <- a[which(a == idx1) + 1] - 2

  # If col_r ("C:" by default) is the LAST variable, idx2 will be empty,
  # if that's the case, we'll take the last row of dfx
  if (length(idx2) == 0) {
    idx2 <- nrow(dfx)
  }

  # Now, we slice dfx in positions idx1 to idx2
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
  # Drop all 0s (there's nothing interesting there)
  varY <- varY[varY$val > 0, ]
  # This split the time.event vector in two
  var_tmp <- do.call(rbind, strsplit(as.character(varY), "\\."))
  # And this creates a dataframe with two columns (time and event)
  var_tmp <- as.data.frame(var_tmp)

  # This assign names to the columns
  if (ncol(var_tmp) > 1) {
    colnames(var_tmp) <- c("tiempo", "evento")
  }
  # Remove 0s from 'tiempo'
  var_tmp <- var_tmp[var_tmp$tiempo > 0, ]
  # This convert variables in numeric class
  for (c in 1:ncol(var_tmp)) {
    var_tmp[, c] <- as.numeric(var_tmp[, c])
  }

  # filesave will be the name of our csv file
  if (save_file) {
    filesave <- sprintf("%sd%s.csv", path_save, fname)
    write.csv(var_tmp,
      file = filesave,
      row.names = FALSE
    )
  }

  # if our is true (T), this return (and save in memory) var_temp
  if (out) {
    return(var_tmp)
  }
}


## Example of use:
# 1) Generate a list of filenames of raw MED data
# 2) Loop over the list with the function, using each element
#    of the list as the fname argument.

# Suppose all raw MED files start with 2020, and you are in the working directory (wd)
# If all the raw MED files are in the wd, we can directly write

# filenames = list.files(pattern = "^2020")

# The above line will look in the wd for all the files starting with "2020"m
# and it will save it as a vector of strings in "filenames".
# With that vector, make a for loop like the following:

# If you want to work immediately with the processed data, first create an empty
# dataframe to store the data file per file
# df_working = data.frame()

# for (f in filenames) {
#   df_tmp = read_med(fname = f,
#                     path_save = "", # put here your path to save the csv
#                     col_r = 'C:', # if you want to process variable C
#                     out = T ) # If you want to store processed data in df_tmp,
# otherwise write out = F
#   df_working = rbind(df_working, df_tmp)
# }

# Thats all.
