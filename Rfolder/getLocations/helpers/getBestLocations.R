#' Flag for and remove erroneous detections
#'
#' @param detects The output of \code{\link{combine_data}}.
#' @param date_bins A matrix of two columns manually specifying the detection periods. The first column represents the detection period start dates whereas the second column represents the detection period end dates. The dates should be in string format "d/m/y." If the date_bins argument is specified, bin_by will be ignored. The function \code{\link{get_date_bins}} is provided to assist in the construction of date_bins.
#' @param bin_by Specifies the length (in days) of the detection periods. If bin_by=2, the best location will be found for each fish in two day bins.
#' @param n_thresh See description.
#' @param dist_max See description.
#' @param remove_flagged If remove_flagged=T, all flagged detections will be removed.
#' @return Returns a list containing two data.frames.
#' $all_detects adds some useful to all_data:
#' \itemize{
#' \item $all_detects$BestSignal is the signal with the highest power in a detection period
#' \item $all_detects$Dist is the Euclidean distance (in km) between the detection location and the associated highest power detection
#' \item $all_detects$FlightNum is the detection period
#' \item $all_detects$Records is number of times that a fish was detected in a detection period
#' }
#' $best_detects contains the highest power detections only:
#' \itemize{
#' \item $best_detects$Flag indicates if a detection has been flagged as erroneous
#' }
#' @description The best location is considered to be the location where the highest power detection occurred. The best location will be flagged if there are fewer than n_thresh detections within a distance of dist_max m from the best detection during the detection period. The best location will also be flagged if a negative linear relationship does not exist between the signal strength and the distance to the highest powered detection for all detections within dist_max km from the best signal in the detection period.
#' @export
#' @examples
#' par(mfrow=c(2,1))
#' make_plot(sldf, river_detects, open_maps=T)
#' best_locations <- get_best_locations(river_detects, date_bins =  date_bins, remove_flagged = T)
#' head(best_locations$all_detects)
#' head(best_locations$best_detects)
#' make_plot(sldf, best_locations$best_detects, open_maps=T)

get_best_locations <- function(detects, date_bins=NA, bin_by = 1, n_thresh=5, dist_max=10000, remove_flagged=F){
  print("Be patient -- this could take a few minutes")
  dist_max <- dist_max/1000
  if (is.na(date_bins[1]) & is.na(bin_by)){
    stop("please supply a value for date_bins or bin_by -- terminating process")
  }else if (!is.na(bin_by) & is.na(date_bins[1])){
    a <- min(detects$DateTime)
    a <- substr(as.character(a, format="%m/%d/%y %H:%M:%OS"),1,8)
    a <- as.POSIXct(a, format="%m/%d/%y", tz="GMT")
    b <- max(detects$DateTime)
    b <- substr(as.character(b, format="%m/%d/%y %H:%M:%OS"),1,8)
    b <- as.POSIXct(b, format="%m/%d/%y", tz="GMT")
    b <- b+86400
    date_mesh <- seq(a, b, by=bin_by*86400)
    dates <- detects$DateTime
    dates <- as.POSIXct(as.character(dates, format="%m/%d/%y"), format="%m/%d/%y", tz="GMT")
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
  }else{
    date_bins <- as.data.frame(date_bins)
    date_bins[] <- lapply(date_bins, as.POSIXct, format="%m/%d/%y", tx="GMT")
    date_bins[,2] <- date_bins[,2]+86400
    date_windows <- date_bins
    names(date_windows) <- c("a", "b")
  }
  detects$BestSignal <- F
  allchannels <- sort(unique(detects$Channel))
  alltags <- sort(unique(detects$TagID))
  detects$Records <- detects$Dist <- detects$FlightNum <- NA
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
          detects$FlightNum[detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi] <- i
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
          best_long <- best_loc$X
          best_lat <- best_loc$Y
          other_locs <- detects[detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi,]
          which <- which(detects$DateTime >= date_windows[i,1] & detects$DateTime < date_windows[i,2] & detects$Channel==channelsi & detects$TagID==tagsi)
          for (i in 1:nrow(other_locs)){
            other_long <- other_locs$X[i]
            other_lat <- other_locs$Y[i]
            detects$Dist[which[i]] <- round(sqrt((best_long[1]-other_long)^2+(best_lat[1]-other_lat)^2)/1000, 3)
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
