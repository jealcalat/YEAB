# Here we show how to process the raw data in batch mode. We assume all
# data to identify subjects, sessions, experiment, and condition are in the
# file name. For example:
# M333_DPY_S01.txt
# contains the data of subject M333, condition "double peak yoked", session 1.

# As can be seen in the raw files, the data has multiple columns. Relevant data
# is stored in arrays starting with a capital letter, then followed by integers
# which are the array indices, starting at zero. For example:

# C:
#      0:        0.120        0.120        0.510        4.120       26.120
#      5:       28.120       32.120       37.120       44.120       52.120

# contains the data of the C array. 0: to 9: (the first 10 values).

# assuming the following directory structure
# .
# ├── csv
# └── raw_med
# on which . is the working directory that we can set with
# setwd('path/to/working/directory')
# raw_med contains the raw data files
# csv contains the processed data files

path_raw_med <- "raw_med"
path_csv <- "csv"
# list.files() returns a character vector of the names of files
list_fnames <- list.files(path = path_raw_med,
  pattern = "(.*).txt", full.names = TRUE)
# if want to read only one array and save it, which has time.event format
for (fname in list_fnames) {
  # read the raw data
  raw_data <- read_med(fname = fname,
    # save the csv file on the csv directory
    save_file = TRUE,
    # the path to save the csv file
    path_save = path_csv,
    # the array to process
    col_r = "C:",
    # return the data.frame to the environment
    out = FALSE,
    # the column names if time_dot_event = TRUE
    col_names = c("time", "event"),
    # this corresponds to DISKCOLUMNS in MED, + 1
    # so, if DISKCOLUMNS = 5, num_col = 6
    num_col = 6,
    # if the time.event vector is saved in variable C,
    # but X might be different
    time_dot_event = TRUE)
}

# if want to read all arrays and save them, there are two options to do so;
# and both involve firs processing the data without saving it, and then
# join the data of every array in a single data.frame. Joining the data
# can be in two ways: by rows or by columns. The first option assumes that both
# arrays have the same number of columns (e.g., 2), the second option assumes
# that both arrays have the same number of rows.

array_list <- c("C:", "X:")

# prepare the data.frame to store the data
df_by_rows <- data.frame()

for (fname in list_fnames) {
  for (arr in array_list) {
    # read the raw data
    raw_data <- read_med(fname = fname,
      # save the csv file on the csv directory
      save_file = FALSE,
      # the path to save the csv file
      path_save = NULL,
      # the array to process
      col_r = arr,
      # return the data.frame to the environment
      out = TRUE,
      # the column names if time_dot_event = TRUE
      col_names = c("time", "event"),
      # this corresponds to DISKCOLUMNS in MED, + 1
      # so, if DISKCOLUMNS = 5, num_col = 6
      num_col = 6,
      # if the time.event vector is saved in variable C,
      # but X might be different
      time_dot_event = FALSE)
    # append the raw data to the data.frame
    raw_data$array <- arr
    df_by_rows <- rbind(df_by_rows, raw_data)
  }
  # sprintf("%s/%s.csv", path_save, sub("\\..*$", "", basename(fname)))
}
