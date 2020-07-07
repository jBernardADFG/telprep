#' flag.dead.fish
#'
#' An add hoc algorithm to determine which fish are alive vs dead.
#'
#' @param best_detects A best detection data.frame. best_detects will generally be output from get.best.locations
#' @param dist_thresh  Basically, if a fish moves less than dist_thresh km for all consecutive detection periods following the detection, the fish will be flagged as dead. Geodesic distance is currently used.
#' @return Returns a data.frame where $MortFlag=T if a fish has been flagged as dead.
#' @export

flag.dead.fish <- function(best_detects, dist_thresh=10){
  allchannels <- sort(unique(best_detects$Channel))
  alltags <- sort(unique(best_detects$TagID))
  new_dets <- data.frame()
  for(channelsi in allchannels) {
    for(tagsi in alltags) {
      dets <- best_detects[best_detects$Channel==channelsi & best_detects$TagID==tagsi,]
      if (nrow(dets)>=1){
        dets <- dets[order(dets$DateTime,decreasing=T),]
        dets$MoveDist <- NA
        if (nrow(dets)>1){
          for (i in 1:(nrow(dets)-1)){
            dets$MoveDist[i] <- round(sqrt((dets$X[i]-dets$X[i+1])^2+(dets$Y[i]-dets$Y[i+1])^2)/1000, 3)
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
