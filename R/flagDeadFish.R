#' Determine the best detections and flag for false detections
#'
#' @description Insert Something
#' @param bd A data.frame containing the raw data from telemetry flights. Detects will generally be the output of combine.data
#' @param dist_thresh Add an attribute to get.best.locations and remove this argument
#' @return Insert something
#' @export

flag.dead.fish <- function(bd, dist_thresh=10){
  allchannels <- sort(unique(bd$Channel))
  alltags <- sort(unique(bd$TagID))
  new_dets <- data.frame()
  for(channelsi in allchannels) {
    for(tagsi in alltags) {
      dets <- bd[bd$Channel==channelsi & bd$TagID==tagsi,]
      if (nrow(dets)>=1){
        dets <- dets[order(dets$DateTime,decreasing=T),]
        dets$MoveDist <- NA
        if (nrow(dets)>1){
          for (i in 1:(nrow(dets)-1)){
            dets$MoveDist[i] <- round(get.geo.dist(dets$Longitude[i], dets$Latitude[i], dets$Longitude[i+1], dets$Latitude[i+1]),2)
          }
        }
      }
      new_dets <- rbind(new_dets, dets)
      new_dets
    }
  }
  rdf <- data.frame()
  new_dets <- new_dets[order(new_dets$DateTime),]
  for(channelsi in allchannels) {
    for(tagsi in alltags) {
      dets <- new_dets[new_dets$Channel==channelsi & new_dets$TagID==tagsi,]
      mort <- "Yes"
      if (nrow(dets)>=1){
        dets <- dets[order(dets$DateTime,decreasing=T),]
        dets$MortFlag <- NA
        if (nrow(dets)==1){
          dets$MortFlag <- "No"
        }else{
          for (i in 1:(nrow(dets)-1)){
            move <- dets$MoveDist[i] > dist_thresh
            if (move){
              mort <- "No"
            }
            dets$MortFlag[i] <- mort
          }
          dets$MortFlag[nrow(dets)] <- "No"
        }
      }
      rdf <- rbind(rdf, dets)
    }
  }
  rdf <- rdf[order(rdf$DateTime),]
  return(rdf)
}
