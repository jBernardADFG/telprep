#' Remove detections that occurred away from the river network
#'
#' @param all_data The output of \code{\link{combine_data}}.
#' @param river_network A river_network object discribing the riversystem topology (the output of \code{\link[riverdist]{line2network}}. The coordinates should use the same projection as the all_data. Note: \emph{attr(all_data, "crs")} the CRS of all_data.
#' @param dist_thresh Distance criteria (in m) to remove detections
#' @return Returns a data.frame containing the detections that occurred less than dist_thresh m from the river network.
#' @export
#' @examples
#' sldf <- sp::spTransform(sldf, attr(all_data, "crs"))
#' par(mfrow=c(2,1))
#' # be patient -- these functions take a few minutes to run
#' make_plot(sldf, all_data, open_maps=T)
#' river_detects <- rm_land_detects(all_data, river_net, dist_thresh = 500)
#' make_plot(sldf, river_detects, open_maps=T)

rm_land_detects <- function(all_data, river_network, dist_thresh=500){
  if (!requireNamespace("telprep", quietly = TRUE)) {
    stop("Package \"telprep\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(river_network$sp@proj4string@projargs != "+proj=utm +zone=5 +datum=WGS84 +units=m +no_defs") stop("the rivernetwork crs needs to be converted to a mercator projection.")
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
