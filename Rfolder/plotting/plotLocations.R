#' plot.locations
#'
#' Plot fish locations
#' @param sldf Shapefile or SpatialLinesDataFrame representation of the river network
#' @param detects Dataframe with the detections -- output of get.best.locations()$best_detects or
#' @param col_by_fish col_by_fish=T assigns fish a unique color
#' @param flight_num vector with the flight numbers to plot. defaults to all
#' @param channel vector with the channel numbers to plot
#' @param tag_id vector with the tag ids to plot
#' @param main plot title
#' @export
make.plot <- function(sldf, detects, open_maps=F, type="bing", darken=4, col_by_fish=F, flight_num=NA, channel=NA, tag_id=NA, viterbi=F, main="", ...){
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
    plot(background, removeMargin=F)
  }else{
    plot(raster::crop(sldf,raster::extent(sldf)), main=main)
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
      print(plot_sym)
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
}
