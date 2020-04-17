#' rm.land.detects
#'
#' Remove the detections that occurred away from the river network
#' @param all_data Channel and date corrected output of combine.data. Note: the coordinates should use the same projection as the river network.
#' @param river_network A river_network object discribing the riversystem topology (the output of riverdist::cleanup or riverdist::line2network).
#' @param crs_1 The all_data coordinate reference system
#' @param crs_2 The river_network coordinate reference system
#' @param dist_thresh Distance criteria (in m) to remove detections
#' @return Returns a dataframe containing the detections that occurred less than dist_thresh m from the river network. Coordinates may be converted. These detections will be plotted on top of the river network.
#' @export

rm.land.detects <- function(all_data, river_network, dist_thresh=500){
  print("be patient -- this could take a few minutes")
  coords <- data.frame(
    lon = all_data$Longitude,
    lat = all_data$Latitude
  )
  snap_dist <- xy2segvert(coords[,1], coords[,2], river_network)$snapdist
  all_data <- all_data[snap_dist<=dist_thresh,]
  par(mar=c(1,1,1,1))
  all_data$Longitude <- coords[snap_dist<=dist_thresh,1]
  all_data$Latitude <- coords[snap_dist<=dist_thresh,2]
  return(all_data)
}
