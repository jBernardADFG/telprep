#' Given a set of flight groupings, determine the start and end dates of cooresponding detection periods
#'
#' @param raw_data The output of \code{\link{read_flight_data}}, \code{\link{channels_merge}}, or \code{\link{replace_date}}.
#' @param flight_group A vector specifying the flight grouping. The ordering of flight_group needs to coorespond to the ordering of raw_data.
#' @return Returns a matrix with two columns. The first column specifies the start dates of the detection periods whereas the second column specifies the end dates of the detection periods. The output will be used as the date_bins argument in the function \code{\link{get_best_locations}}.
#' @export
#' @examples
#' names(raw_data)
#' flight_group <- flight_group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,5,5,6,6,7,7,7,7,8,8)
#' date_bins <- get_date_bins(raw_data, flight_group)
#' date_bins

get_date_bins <- function(raw_data, flight_group){
  r_mat <- matrix(NA, nrow=length(unique(flight_group)), ncol=2)
  for (i in 1:length(raw_data)){
    min <- min(raw_data[[i]]$DateTime)
    max <- max(raw_data[[i]]$DateTime)
    if (is.na(r_mat[flight_group[i],1])){
      r_mat[flight_group[i],1] <- as.character(min, format="%m/%d/%y")
    }else if(as.POSIXct(r_mat[flight_group[i],1], format="%m/%d/%y") > min){
      r_mat[flight_group[i],1] <- as.character(min, format="%m/%d/%y")
    }
    if (is.na(r_mat[flight_group[i],2])){
      r_mat[flight_group[i],2] <- as.character(max, format="%m/%d/%y")
    }else if(as.POSIXct(r_mat[flight_group[i],2], format="%m/%d/%y") < max){
      r_mat[flight_group[i],2] <- as.character(max, format="%m/%d/%y")
    }
  }
  return(r_mat)
}
