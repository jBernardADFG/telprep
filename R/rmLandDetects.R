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
  if (!requireNamespace("telprep", quietly = TRUE)) {
    stop("Package \"telprep\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(rivernet$sp@proj4string@projargs != "+proj=utm +zone=5 +datum=WGS84 +units=m +no_defs") stop("the rivernetwork crs needs to be converted to a mercator projection.")
  print("be patient -- this could take a few minutes")
  coords <- data.frame(
    lon = all_data$X,
    lat = all_data$Y
  )
  snap_dist <- riverdist::xy2segvert(coords[,1], coords[,2], river_network)$snapdist
  all_data <- all_data[snap_dist<=dist_thresh,]
  par(mar=c(1,1,1,1))
  all_data$X <- coords[snap_dist<=dist_thresh,1]
  all_data$Y <- coords[snap_dist<=dist_thresh,2]
  return(all_data)
}
