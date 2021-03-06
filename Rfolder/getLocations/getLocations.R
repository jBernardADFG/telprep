#' Automatically determine the location of each fish in each detection period
#'
#' @param raw_data The output of \code{\link{read_flight_data}}, \code{\link{channels_merge}}, or \code{\link{replace_date}}
#' @param flight_group A numerical vector encoding the detection period (flight group) associated with each data.frame in raw.data.
#' @param river_network A river_network object discribing the riversystem topology (the output of \code{\link[riverdist]{line2network}}).
#' @param n_d See description
#' @param d_1 See description
#' @param d_2 See description
#' @description A two-stage routine is used to remove erroneous detections and determine the best detection for each fish in each study period. In removing erroneous signals and determining the location of fish, the function draws from the functionality of the R packages riverdist. The function combined the contents of raw_data, removing the detections that occurred d_1 meters away from the river system, and determines the location of each fish in each detection period. In short, the fish location is considered to be the location on the river network where the highest power detection occurred for each fish in each detection period subject to two constraints: 1) n_d other detections must occur within d_2 meters from the highest power detection, and 2) there must be a negative linear relationship between the signal strength and the distance between the detection and associated the highest power detection the distance from the highest powered detection for all detections occurrring within d_2 meters from the highest power detection. In simpler terms, the signal stength increases as the highest power detection is approached.
#' @return Returns a data.frame containing the best locations. The data.frame can be used as input to the following functions: \code{\link{make_plot}}, \code{\link{flag_dead_fish}}, and \code{\link{hmm_survival}}.
#' @export
#' @examples
#' names(raw_data)
#' flight_group <- flight_group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,5,5,6,6,7,7,7,7,8,8)
#' source_vec <- c(rep(c("belly","wing"), 10), rep(c("wing","belly"),2), c("belly","wing"))
#' best_locations <- get_locations(raw_data, flight_group, source_vec, river_net, n_d=5, d_1=1000, d_2=5000)
#' make_plot(sldf, best_locations, open_maps=T)


get_locations <- function(raw_data, flight_group, source_vec, river_network, n_d, d_1, d_2){
  all_detects <- combine_data(raw_data, source_vec)
  river_detects <- rm_land_detects(all_detects, river_network, d_1)
  date_bins <- get_date_bins(raw_data, flight_group)
  best_detections <- get_best_locations(river_detects, date_bins, n_thresh = n_d , dist_max = d_2, remove_flagged = T)
  locations <- best_detections$best_detects
  locations <- locations[,which(is.element(names(locations), c("Channel", "TagID", "X", "Y", "Status", "FlightNum")))]
  return(locations)
}
