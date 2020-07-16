#' A general plotting function
#'
#' @param sldf Shapefile or SpatialLinesDataFrame representation of the river network.
#' @param detects Output of \code{\link{combine_data}}, \code{\link{rm_land_detects}}, \code{\link{get_best_locations}}, \code{\link{flag_dead_fish}}, \code{\link{flag_dead_fish}}, or \code{\link{hmm_survival}}.
#' @param extent A vector of length four specifying the plotting extent c(x_min, x_max, y_min, y_max)
#' @param type The background to use (see \code{\link[OpenStreetMap]{openmap}}) for more information.
#' @param darken Increase to darken the background when open_maps=T. Defaults to 1.
#' @param col_by_fish col_by_fish=T assigns each fish a unique color. This color will be preserved between mappings (i.e. between different flight periods).
#' @param flight_num Numerical argument specifying the flight period to plot. Defaults to all.
#' @param channel Vector with the channel numbers to plot. If channel=NA, all channels will be used.
#' @param tag_id Vector with the tag ids to plot. If tag_id=NA, all tag ids will be used.
#' @param viterbi Use viterbi=T to color by survival state using the viterbi path (detects needs to be the viterbi output from \code{\link{hmm_survival}}; see examples). Expired fish will be plotted in green.
#' @export
#' @examples
#' # plotting all detections
#' par(mfrow=c(1,1))
#' make_plot(sldf, all_data)
#'
#' # real detections only
#' make_plot(sldf, best_detects)
#'
#' # darken background
#' make_plot(sldf, best_detects, darken=2.5)
#'
#' # change style of background
#' make_plot(sldf, best_detects, type="esri-topo")
#'
#' # give each fish a unique color preserved through flights -- unfortunately there are only so many colors
#' par(mfrow=c(3,1))
#' make_plot(sldf, best_detects, col_by_fish=T, flight=1, darken=2.5)
#' make_plot(sldf, best_detects, col_by_fish=T, flight=2, darken=2.5)
#' make_plot(sldf, best_detects, col_by_fish=T, flight=3, darken=2.5)
#'
#' # to plot the locations for a single fish
#' par(mfrow=c(1,1))
#' make_plot(sldf, best_detects, channel=10, tag_id=11, darken=2.5)
#'
#' # to zoom in to a specified extent
#' extent <- c(x_min=466060, x_max=1174579, y_min=6835662, y_max=7499016)
#' temp<-make_plot(sldf, best_detects, extent, darken=2.5)
#'
#' # plotting live and dead fish by flight period -- green fish have expired
#' par(mfrow=c(3,1))
#' viterbi <- hmm_survival(best_detects)$viterbi
#' make_plot(sldf, viterbi, type="bing", darken=2.5, viterbi=T, flight=1)
#' make_plot(sldf, viterbi, type="bing", darken=2.5, viterbi=T, flight=3)
#' make_plot(sldf, viterbi, type="bing", darken=2.5, viterbi=T, flight=5)


make_plot <- function(sldf, detects, extent=NA, type="bing", darken=1, col_by_fish=F, flight_num=NA, channel=NA, tag_id=NA, viterbi=F, return_background=F){
  open_maps=T
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("Package \"sp\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package \"raster\" is needed. Please install it.",
         call. = FALSE)
  }
  if (is.na(flight_num[1])){
    flight_num <- unique(detects$FlightNum)
  }
  if (is.na(channel[1])){
    channel <- unique(detects$Channel)
  }
  if (is.na(tag_id[1])){
    tag_id <- unique(detects$TagID)
  }
  par(mar=c(1,1,1,1))
  if (!is.na(extent)[1]){
    sldf@bbox[1,1] <- extent[1]
    sldf@bbox[1,2] <- extent[2]
    sldf@bbox[2,1] <- extent[3]
    sldf@bbox[2,2] <- extent[4]
  }
  sldf <- crop(sldf, sldf@bbox)
  if(isTRUE(open_maps)){
    if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
      stop("Package \"OpenStreetMap\" is needed when open_maps=T. Please install it.",
           call. = FALSE)
    }
    if (!requireNamespace("plotwidgets", quietly = TRUE)) {
      stop("Package \"plotwidgets\" is needed when open_maps=T. Please install it.",
           call. = FALSE)
    }
    bbox <- sp::spTransform(sldf, sp::CRS("+init=epsg:4326"))@bbox
    background <- suppressWarnings(OpenStreetMap::openmap(upperLeft=c(bbox[2,2],bbox[1,1]), lowerRight = c(bbox[2,1],bbox[1,2]),
                                                          type=type))
    rgb <- background$tiles[[1]]$colorData
    hsl <- plotwidgets::col2hsl(rgb)
    hsl[3,] <- hsl[3,]/darken
    background$tiles[[1]]$colorData <- plotwidgets::hsl2col(hsl)
    plot(background, main="",removeMargin=F)
  }else{
    plot(raster::crop(sldf,raster::extent(sldf)), main="")
    rect(sldf@bbox[1,1],sldf@bbox[2,1],sldf@bbox[1,2],sldf@bbox[2,2], col = "black")
  }
  if (is.null(detects$FlightNum)){
    bd <- detects[is.element(detects$Channel, channel) & is.element(detects$TagID, tag_id),]
  }else{
    bd <- detects[is.element(detects$FlightNum, flight_num) & is.element(detects$Channel, channel) & is.element(detects$TagID, tag_id),]
  }
  x_vec <- c(sldf@bbox[1,1],sldf@bbox[1,1],sldf@bbox[1,2],sldf@bbox[1,2],sldf@bbox[1,1])
  y_vec <- c(sldf@bbox[2,1],sldf@bbox[2,2],sldf@bbox[2,2],sldf@bbox[2,1],sldf@bbox[2,1])
  bd <- bd[sp::point.in.polygon(bd$X, bd$Y, x_vec, y_vec)==1,]
  c <- data.frame(bd$X, bd$Y)
  sp <- sp::SpatialPoints(c, raster::crs(sldf))
  new_sp <- sp::spTransform(sp, background[[1]][[1]]$projection)
  bd$X <- new_sp@coords[,1]
  bd$Y <- new_sp@coords[,2]
  sldf <- sp::spTransform(sldf, background[[1]][[1]]$projection)
  if (col_by_fish){
    set.seed(3)
    color_mat <- matrix(NA, nrow=100, ncol=100)
    for(i in 1:100){
      for(j in 1:100){
        samp <- sample(550:650, 1)
        color_mat[i,j] <- colors()[samp]
      }
    }
    cols <- rep(NA, nrow(bd))
    for(i in 1:nrow(bd)){
      cols[i] <- color_mat[bd$Channel[i], bd$TagID[i]]
    }
    if(sum(names(detects)=="MortFlag")==0){
      lines(raster::crop(sldf,raster::extent(sldf)), col="blue4", lwd=2)
      points(bd$X, bd$Y, pch=19, col=cols, cex=1, xlim=bbox(sldf)[1,],ylim=bbox(sldf)[2,])
    }else{
      plot_sym <- rep(19, nrow(bd))
      plot_sym[bd$MortFlag=="Yes"] <- 4
      lines(raster::crop(sldf,raster::extent(sldf)), col="blue4", lwd=2)
      points(bd$X, bd$Y, pch=plot_sym, col=cols, cex=1, xlim=bbox(sldf)[1,],ylim=bbox(sldf)[2,])
    }
  }else if (viterbi==T){
    plot_sym <- rep(19, nrow(bd))
    plot_col_1 <- rep("red", nrow(bd))
    plot_col_1[bd$Viterbi==2] <- "green4"
    plot_col_2 <- rep("orange", nrow(bd))
    plot_col_2[bd$Viterbi==2] <- "green3"
    plot_col_3 <- rep("yellow", nrow(bd))
    plot_col_3[bd$Viterbi==2] <- "green2"
    lines(raster::crop(sldf,raster::extent(sldf)), col="blue4", lwd=2)
    points(bd$X, bd$Y, pch=plot_sym, col=plot_col_1, cex=1)
    points(bd$X, bd$Y, pch=plot_sym, col=plot_col_2, cex=0.5)
    points(bd$X, bd$Y, pch=plot_sym, col=plot_col_3, cex=0.1)
  }else{
    if(sum(names(detects)=="MortFlag")==0){
      lines(raster::crop(sldf,raster::extent(sldf)), col="blue4", lwd=2)
      points(bd$X, bd$Y, pch=19, col="red", cex=1)
      points(bd$X, bd$Y, pch=19, col="orange", cex=0.5)
      points(bd$X, bd$Y, pch=19, col="yellow", cex=0.1)
    }else{
      plot_sym <- rep(19, nrow(bd))
      plot_col_1 <- rep("red", nrow(bd))
      plot_col_1[bd$MortFlag=="Yes"] <- "green4"
      plot_col_2 <- rep("orange", nrow(bd))
      plot_col_2[bd$MortFlag=="Yes"] <- "green3"
      plot_col_3 <- rep("yellow", nrow(bd))
      plot_col_3[bd$MortFlag=="Yes"] <- "green2"
      lines(raster::crop(sldf,raster::extent(sldf)), col="blue4", lwd=2)
      points(bd$X, bd$Y, pch=plot_sym, col=plot_col_1, cex=1)
      points(bd$X, bd$Y, pch=plot_sym, col=plot_col_2, cex=0.5)
      points(bd$X, bd$Y, pch=plot_sym, col=plot_col_3, cex=0.1)
    }
  }
  if (return_background==T){
    return(background)
  }
}

