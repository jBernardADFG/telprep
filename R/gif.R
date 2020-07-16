#' Create a dynamic plot of fish detections
#'
#' @param sldf A SpatialLineDataFrame representation of the river system. The CRS should match that of the detection data.
#' @param detects Output of \code{\link{get_locations}} or \code{\link{get_best_locations}}
#' @param dir The directory of the folder where the plots should be output (eg. "D:/Jordy/myplots/"). The folder should end with a "/".
#' @param extent A vector of length four specifying the plotting extent c(x_min, x_max, y_min, y_max)
#' @param type The background to use (see \code{\link[OpenStreetMap]{openmap}}) for more information.
#' @param darken Increase to darken the background when open_maps=T. Defaults to 1.
#' @param col_by_fish col_by_fish=T assigns each fish a unique color. This color will be preserved between mappings (i.e. between different flight periods).
#' @param viterbi Use viterbi=T to color by survival state using the viterbi path (detects needs to be the viterbi output from \code{\link{hmm_survival}}; see examples). Expired fish will be plotted in green.
#' @param width The width of the plot.
#' @param height The height of the plot.
#' @return Static plots that plot the fish location by flight will be added to the folder along with a gif that iterates through the flights.
#' @export
#' @examples
#' # Note: Create a folder on your machine and make dir coorespond to this directory to run examples
#' # extent <- c(x_min=466060, x_max=1174579, y_min=6835662, y_max=7499016)
#' # gif_plot(sldf, viterbi, dir="S:/Jordy/telprep/telprep/gifs/viterbi/", extent=extent, viterbi=T)
#' # gif_plot(sldf, best_detects, dir="S:/Jordy/telprep/telprep/gifs/byfish/", extent=extent, col_by_fish=T, viterbi=F)

gif_plot <- function(sldf, detects, dir = "D:/Jordy/myplots/", extent=NA, type="bing", darken=2.5, col_by_fish=F, viterbi=F, width = 1024, height=768, fps = fps){
  par(mfrow=c(1,1))
  n_flights <- length(unique(detects$FlightNum))
  files <- rep(NA,n_flights)
  for (i in 1:n_flights){
    base_map <- make_plot(sldf, detects, flight=i, extent=extent, type=type, darken=darken, col_by_fish=col_by_fish, viterbi=viterbi)
    x_min <- base_map$bbox$p1[1]
    x_max <- base_map$bbox$p2[1]
    y_min <- base_map$bbox$p2[2]
    y_max <- base_map$bbox$p1[2]
    x_mid <- (x_max+x_min)/2
    y_diff <- y_max-y_min
    text(x=x_mid, y=y_max-y_diff/8, labels=paste("Flight", i, sep=" "), col="yellow", cex=4)
    file <- paste(dir, "plot", i, ".png", sep="")
    files[i] <- file
    dev.print(png, file = file, width = width, height = height)
    dev.off()
  }
  images <- purrr::map(files, image_read)
  images <- image_join(images)
  animation <- image_animate(images, fps = 0.5)
  image_write(animation, paste(dir, "gif.gif", sep=""))
}


