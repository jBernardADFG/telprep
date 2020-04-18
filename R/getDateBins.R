#' get.date.bins
#'
#' Get the detection periods a set of flight groupings
#' @param raw_data the date and channel corrected output of read.flight.data
#' @param flight_group a vector specifying the flight grouping. The ordering of flight_group cooresponds to the ordering of raw_data.
#' @return returns a matrix with two columns. The first column specifies the start dates of the detection periods whereas the second column specifies the end dates of the detection periods. The output will be used as the date_bins argument in the function get.best.locations.
#' @export

get.date.bins <- function(raw_data, flight_group){
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
