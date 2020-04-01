#' Modify a channel if it was programmed incorrectly
#' @export
#' @param data data.frame output from read.flight.folder
#' @param replace The channel to replace
#' @param with The replacement channel
#' @return
channels.merge <- function(data, replace, with){
  data$Channel[data$Channel==replace] <- with
  return(raw_data)
}
