#' Modify a date if it was programmed incorrectly
#'
#' @param data data.frame output from read.flight.folder
#' @param new_date String representation of the replacement date. The date should be formatted as \%m/\%d/\%y" eg: 12/25/20.
#' @return insert info
#' @export

replace.date <- function(data, new_date){
  date_string <- as.character(data$DateTime, format="%m/%d/%y %H:%M:%OS")
  date_string <- paste(new_date, substr(date_string, 10,17), sep=" ")
  data$DateTime <- as.POSIXct(date_string, format="%m/%d/%y %H:%M:%OS")
  return(data)
}
