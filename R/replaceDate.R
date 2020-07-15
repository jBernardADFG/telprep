#' Modify a date if it was programmed incorrectly
#'
#' @param data The output of \code{\link{read_flight_data}} or \code{\link{channels_merge}}.
#' @param new_date String representation of the replacement date. The date should be formatted as "\%m/\%d/\%y" (eg: "12/25/20").
#' @export
#' @examples
#' head(raw_data[[16]])
#' raw_data[[16]] <- replace_date(raw_data[[16]], new_date ="10/17/19")
#' head(raw_data[[16]])

replace_date <- function(data, new_date){
  date_string <- as.character(data$DateTime, format="%m/%d/%y %H:%M:%OS")
  date_string <- paste(new_date, substr(date_string, 10,17), sep=" ")
  data$DateTime <- as.POSIXct(date_string, format="%m/%d/%y %H:%M:%OS")
  return(data)
}
