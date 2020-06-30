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
plot.locations <- function(sldf, detects, col_by_fish=T, flight_num=NA, channel=NA, tag_id=NA, viterbi=F, main=""){
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
  plot(crop(sldf,extent(sldf)), main=main)
  rect(sldf@bbox[1,1],sldf@bbox[2,1],sldf@bbox[1,2],sldf@bbox[2,2], col = "black")
  # lines(crop(sldf,extent(sldf)), col="back", lwd=1)
  if (is.null(detects$FlightNum)){
    bd <- detects[is.element(detects$Channel, channel) & is.element(detects$TagID, tag_id),]
  }else{
    bd <- detects[is.element(detects$FlightNum, flight_num) & is.element(detects$Channel, channel) & is.element(detects$TagID, tag_id),]
  }
  x_vec <- c(sldf@bbox[1,1],sldf@bbox[1,1],sldf@bbox[1,2],sldf@bbox[1,2],sldf@bbox[1,1])
  y_vec <- c(sldf@bbox[2,1],sldf@bbox[2,2],sldf@bbox[2,2],sldf@bbox[2,1],sldf@bbox[2,1])
  bd <- bd[point.in.polygon(bd$Longitude, bd$Latitude, x_vec, y_vec)==1,]
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
      lines(crop(sldf,extent(sldf)), col="blue4", lwd=2)
      points(bd$Longitude, bd$Latitude, pch=19, col=cols, cex=1, xlim=bbox(sldf)[1,],ylim=bbox(sldf)[2,])
    }else{
      # Needs to be unfucked
      plot_sym <- rep(19, nrow(bd))
      plot_sym[bd$MortFlag=="Yes"] <- 4
      print(plot_sym)
      lines(crop(sldf,extent(sldf)), col="blue4", lwd=2)
      points(bd$Longitude, bd$Latitude, pch=plot_sym, col=cols, cex=1, xlim=bbox(sldf)[1,],ylim=bbox(sldf)[2,])
    }
  }else if (viterbi==T){
    plot_sym <- rep(19, nrow(bd))
    plot_col_1 <- rep("red", nrow(bd))
    plot_col_1[bd$Viterbi==2] <- "green4"
    plot_col_2 <- rep("orange", nrow(bd))
    plot_col_2[bd$Viterbi==2] <- "green3"
    plot_col_3 <- rep("yellow", nrow(bd))
    plot_col_3[bd$Viterbi==2] <- "green2"
    lines(crop(sldf,extent(sldf)), col="blue4", lwd=2)
    points(bd$Longitude, bd$Latitude, pch=plot_sym, col=plot_col_1, cex=1)
    points(bd$Longitude, bd$Latitude, pch=plot_sym, col=plot_col_2, cex=0.5)
    points(bd$Longitude, bd$Latitude, pch=plot_sym, col=plot_col_3, cex=0.1)
  }else{
    if(sum(names(detects)=="MortFlag")==0){
      lines(crop(sldf,extent(sldf)), col="blue4", lwd=2)
      points(bd$Longitude, bd$Latitude, pch=19, col="red", cex=1)
      points(bd$Longitude, bd$Latitude, pch=19, col="orange", cex=0.5)
      points(bd$Longitude, bd$Latitude, pch=19, col="yellow", cex=0.1)
    }else{
      plot_sym <- rep(19, nrow(bd))
      plot_col_1 <- rep("red", nrow(bd))
      plot_col_1[bd$MortFlag=="Yes"] <- "green4"
      plot_col_2 <- rep("orange", nrow(bd))
      plot_col_2[bd$MortFlag=="Yes"] <- "green3"
      plot_col_3 <- rep("yellow", nrow(bd))
      plot_col_3[bd$MortFlag=="Yes"] <- "green2"
      lines(crop(sldf,extent(sldf)), col="blue4", lwd=2)
      points(bd$Longitude, bd$Latitude, pch=plot_sym, col=plot_col_1, cex=1)
      points(bd$Longitude, bd$Latitude, pch=plot_sym, col=plot_col_2, cex=0.5)
      points(bd$Longitude, bd$Latitude, pch=plot_sym, col=plot_col_3, cex=0.1)
    }
  }
}
