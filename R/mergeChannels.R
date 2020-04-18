#' channels.merge
#'
#' Modify a channel if it was programmed incorrectly
#' @param data data.frame output from read.flight.folder
#' @param replace The channel to replace
#' @param with The replacement channel
#' @return Returns the input data.frame with a modified channel
#' @export

channels.merge <- function(data, replace, with){
  data$Channel[data$Channel==replace] <- with
  return(data)
}
