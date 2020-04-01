#' Determine the best detections and flag for false detections
#'
#' @description Insert Something
#' @param detects A data.frame containing the raw data from telemetry flights. Detects will generally be the output of combine.data
#' @param bin_by Specifies the length (in days) of the detection periods. If bin_by=2, the best location will be found for each fish in two day bins.
#' @param n_thresh Argument used to flag for false detections.
#' @param dist_max Argument used to flag for false detections.
#' @param remove_flagged If remove_flagged=T, the flagged detections will be removed in the returned data.frame.
#' @return Returns a list containing two data.frames. $all_detects contains... $best_detects contains...
#' @export

get.best.locations <- function(detects, bin_by = 1, n_thresh=5, dist_max=10, remove_flagged=F){
  print("Be patient -- this could take a few minutes")
  a <- min(detects$DateTime)
  a <- substr(as.character(a, format="%m/%d/%y %H:%M:%OS"),1,8)
  a <- as.POSIXct(a, format="%m/%d/%y")
  b <- max(detects$DateTime)
  b <- substr(as.character(b, format="%m/%d/%y %H:%M:%OS"),1,8)
  b <- as.POSIXct(b, format="%m/%d/%y")
  b <- b+86400
  date_mesh <- seq(a, b, by=bin_by*86400)
  dates <- detects$DateTime
  dates <- as.POSIXct(as.character(dates, format="%m/%d/%y"), format="%m/%d/%y")
  dates <- unique(dates)
  dates <- dates + 43200
  bool <- rep(F, length(date_mesh)-1)
  for (i in 1:length(dates)){
    date <- dates[i]
    for (j in 1:length(bool)){
      if (date >= date_mesh[j] & date < date_mesh[j+1]){
        bool[j] <- T
      }
    }
  }
  date_windows <- data.frame()
  for (i in 1:length(bool)){
    if (bool[i] == T){
      date_windows <- rbind(date_windows, data.frame(a=date_mesh[i],b=date_mesh[i+1]))
    }
  }
  detects$BestSignal <- F
  allchannels <- sort(unique(detects$Channel))
  alltags <- sort(unique(detects$TagID))
  detects$Records <- detects$Dist <- NA
  tt <- 1
  for(channelsi in allchannels) {
    print(round(tt/(3*length(allchannels)),2))
    tt <- tt + 1
    for(tagsi in alltags){
      for (i in 1:nrow(date_windows)){
        date_bin <- date_windows[i,]
        if(sum(detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi) > 0) {
          maxpower <- max(detects$Power[detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi])
          detects$BestSignal[detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi & detects$Power==maxpower][1] <- T
          detects$Records[detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi] <- sum(detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi)
          detects$Records[detects$TagID==999] <- NA
        }
      }
    }
  }
  locs <- detects[detects$TagID!=999,]
  best_locs <- subset(locs, BestSignal)
  best_locs$BestSignal <- NULL
  for(channelsi in allchannels) {
    print(round(tt/(3*length(allchannels)),2))
    tt <- tt + 1
    for(tagsi in alltags) {
      for (i in 1:nrow(date_windows)){
        if(sum(detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi) > 0) {
          best_loc <- detects[detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi & detects$BestSignal==T,]
          best_long <- best_loc$Longitude
          best_lat <- best_loc$Latitude
          other_locs <- detects[detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi,]
          which <- which(detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi)
          for (i in 1:nrow(other_locs)){
            other_long <- other_locs$Longitude[i]
            other_lat <- other_locs$Latitude[i]
            detects$Dist[which[i]] <- round(get.geo.dist(best_long, best_lat, other_long, other_lat),3)
          }
        }
      }
    }
  }
  all_detects <- detects
  best_detects <- detects
  best_detects <- all_detects[all_detects$BestSignal==T,]
  best_detects <- best_detects[, names(best_detects)!="BestSignal" & names(best_detects)!= "Dist" & names(best_detects)!= "Antenna"]
  best_detects$flag <- T
  for(channelsi in allchannels) {
    print(round(tt/(3*length(allchannels)),2))
    tt <- tt + 1
    for(tagsi in alltags) {
      for (i in 1:nrow(date_windows)){
        if(sum(all_detects$DateTime >= date_windows[i,1] & all_detects$DateTime < date_windows[i,2] & all_detects$Channel==channelsi & all_detects$TagID==tagsi) > 0) {
          detects <- all_detects[all_detects$DateTime >= date_windows[i,1] & all_detects$DateTime < date_windows[i,2] & all_detects$Channel==channelsi & all_detects$TagID==tagsi,]
          detects <- detects[detects$Dist < dist_max,]
          flag <- T
          if (nrow(detects) > n_thresh){
            mod <- lm(detects$Power~detects$Dist)
            if(mod$coefficients[2] < 0){
              flag <- F
            }
          }
          best_detects$flag[best_detects$DateTime >= date_windows[i,1] & best_detects$DateTime < date_windows[i,2] & best_detects$Channel==channelsi & best_detects$TagID==tagsi] <- flag
        }
      }
    }
  }
  if (remove_flagged){
    best_detects <- best_detects[!best_detects$flag,]
    best_detects <- best_detects[,names(best_detects)!="Flagged"]
  }
  return(list(all_detects=all_detects, best_detects=best_detects))
}

