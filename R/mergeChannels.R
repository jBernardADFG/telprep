#' Modify a channel if it was programmed incorrectly
#'
#' @param data The output of \code{\link{read_flight_data}} or \code{\link{replace_date}}.
#' @param replace The channel to replace.
#' @param with The replacement channel.
#' @return Returns a channel modified data.frame.
#' @export
#' @examples
#' names(raw_data)
#' head(raw_data[[2]])
#' raw_data[[2]] <- channels_merge(raw_data[[2]], 10, 3)
#' head(raw_data[[2]])

channels_merge <- function(data, replace, with){
  data$Channel[data$Channel==replace] <- with
  return(data)
}

