# event_extractor.R
# Description:
#   A function to slice MED data based on start and stop events. This function
#   should be used after read_med.r, which outputs a csv of 2 columns: time and
#   events (in that order). Its use is exemplified at the end of the function.

# Creation: April 2020, during a pandemic
#   Author: Emmanuel Alcala

# Inputs:
#   data_df: a dataframe of m rows x 2 columns, where columns corresponds
#           to time and events IDs, in that order.
#   ev0: event ID start (where the event we want to extract begins)
#   ev1: event ID stop. This event won't be returned, so keep in mind that
#   evname: a string for the event name, for identification purposes. For example
#           if the event we want to extract is component 1 in a multiple-2 sche-
#           dule, this can be eventname = "c1", so when we extract the second
#           component we can row-combine both in a unique dataframe.
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Outputs:
#   dftmp: data frame with j x 4 columns of time, events, cum_id and evname

#' @title Event extractor
#'
#' @description  A function to slice data based on start and stop events. This function
#'   should be used after read_med.r, which outputs a csv of 2 columns: time and
#'   events (in that order). Its use is exemplified at the end of the function.
#'
#' @param data_df data frame with events ev0 and ev1 (e.g., start of trial and reinforcement delivery)
#' @param ev0 event ID start (where the event we want to extract begins)
#' @param ev1 event ID stop. This event won't be returned, so keep in mind that
#' @param evname a string for the event name, for identification purposes. For example
#'    if the event we want to extract is component 1 in a multiple-2 schedule,
#'    this can be eventname = "c1", so when we extract the second
#'    component we can row-combine both in a unique dataframe.
#' @return data frame with nrows x 4 columns of time, events, cum_id and evname
#' @export
#' @details Works by trials
#' @examples
#' # If we have a component starting with 5 and ending with 3, 
#' # say a Fixed Interval 15s and a dataframe of events from the read_med() function,
#' # we can extract the data of component "FI15" following the next steps:
#' # 0 - From the output of read_med.R function, load the extracted data and assign it to df
#' # 1 - source the event_extractor.R function
#' # 2 - use it with the appropiate arguments as follows
#' 
#' # read raw data from MED
#' data("fi60_raw_from_med")
#' # see first 10 lines
#' head(fi60_raw_from_med, 10)
#' # create a temporary file to avoid non-staged installation warning
#' temp_file <- tempfile(fileext = ".txt")
#' # write the data to the temporary file
#' writeLines(fi60_raw_from_med, temp_file)
#' # Process the file using read_med
#' example_processed <- read_med(
#'   fname = temp_file, save_file = FALSE,
#'   col_r = "C:", out = TRUE,
#'   col_names = c("time", "event"), num_col = 6, time_dot_event = TRUE
#' )
#' 
#' # Extract specific events (FI15 in this case)
#' extracted_FI15 <- event_extractor(
#'   data_df = example_processed,
#'   ev0 = 5, ev1 = 3,
#'   evname = "FI15"
#' )
#' 
#' # Display the first rows of the extracted data
#' head(extracted_FI15, 30)
#' 
event_extractor <- function(data_df, ev0, ev1, evname) {
  evs <- c(ev0, ev1)

  # Boolean variable where there is either an ev0 or ev1
  mark.v <- ifelse(data_df[, 2] %in% evs, 1, 0)
  # Make a cumulative sum of events
  data_df$cum_id <- cumsum(mark.v)
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Slice the event based on cumsum of start and end. For an alternative, see
  ## 'alternative slicing' at the end of the script.
  # The above lines make a vector (cum_id) of integers with odd numbers corresponding
  # to the event we want to extract. It starts to count 1 where there is ev0,
  # and 2 when there is ev1, 3 when ev0 again. So, even numbers corresponds to
  # the end of the events of interest. We will use this information next, using
  # the %% (module) operator, which returns the remainder of a division (not the
  # result of the division).  4 %% 2 equals 0, while 4 / 2 = 2

  # The operation x %% 2 == 1 evaluates if x/2 has a remainder of 1, or if is
  # an exact multiple of 2 (remainder of 0). This will make a boolean variable
  # that we'll use to slice data in the form data_df[TRUE, ]

  # 1
  event_remover <- data_df$cum_id %% 2 == 1
  # 2
  dftmp <- data_df[event_remover, ]

  # dftmp[, 4] <- evname
  dftmp$evname <- evname
  # return dftmp
  dftmp
}
