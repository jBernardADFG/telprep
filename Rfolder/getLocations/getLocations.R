#' get.locations
#'
#' Determine the location of each fish in each detection period
#' @description A two-stage routine is used to remove erroneous detections and determine the best detection for each fish in each study period. In removing erroneous signals and determining the location of fish, the function draws from the functionality of the R packages riverdist. The function combined the contents of raw_data, removing the detections that occurred $d_1$ meters away from the river system, and determines the location of each fish in each detection period. In short, the fish location is considered to be the location on the river network where the highest power detection occurred for each fish in each detection period subject to two constraints: (1) $n_d$ other detections must occur within $d_2$ meters from the highest power detection, and (2) there must be a negative linear relationship between the signal strength and $d_3$ for all detections within $d_2$ meters from the highest power detection where $d_3$ is defined as the Euclidean distance between the detection and associated the highest power detection. To state constraint (2) in layman's terms, the signal stength increases as the highest power detection is approached.
#' @param raw_data A list of data.frames containing the date and channel corrected output of read.raw.data
#' @param flight_group A numerical vector encoding the detection period (flight group) associated with each data.frame in raw.data (see examples).
#' @param river_network A river_network object discribing the riversystem topology (the output of riverdist::line2network; call help(line2network)).
#' @param n_d See discription; n_d=$n_d$
#' @param d_1 See discription; d_1=$d_1$
#' @param d_2 See discription; d_2=$d_2$
#' @return Returns a data.frame containing information related to the fish locations. This data.frame can be used as input for the functions plot.locations and hmm.survival (call help(plot.locations) and help(hmm.survival)).
#' @export
#' @examples

get.locations <- function(raw_data, flight_group, river_network, n_d, d_1, d_2){
  all_detects <- combine.data(raw_data, source_vec)
  river_detects <- rm.land.detects(all_detects, river_network, d_1)
  date_bins <- get.date.bins(raw_data, flight_group)
  best_detections <- get.best.locations(river_detects, date_bins, n_thresh = n_d , dist_max = d_2, remove_flagged = T)
  locations <- best_detections$best_detects
  locations <- locations[,which(is.element(names(locations)), c("Channel", "TagID", "Latitude", "Longitude", "Status", "FlightNum"))]
  return(locations)
}
