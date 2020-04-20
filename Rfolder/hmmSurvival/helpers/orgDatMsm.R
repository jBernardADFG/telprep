#' org.dat.msm
#'
#' Function to extract relevant information to fit hmm using msm::msm
#' @param best_detects get.best.locations()$best_detects
#' @return returns a data.frame

org.dat.msm <- function(best_detects, t_star=1){
  best_detects <- best_detects[order(best_detects$DateTime),]
  flight_nums <- best_detects$FlightNum
  med_times <- rep(NA, length(unique(flight_nums)))
  for (i in unique(flight_nums)){
    med_times[i] <- median(best_detects$DateTime[flight_nums==i])
  }
  med_times <- floor((med_times-min(med_times))/86400)+1
  times_conversion_mat <- cbind(unique(flight_nums), med_times)
  best_detects$Subject <- best_detects$Day <-  best_detects$Disp <- NA
  channels <- sort(unique(best_detects$Channel))
  ids <- sort(unique(best_detects$TagID))
  k <- 1
  for (channelsi in channels){
    for (idsi in ids){
      if (sum(best_detects$Channel == channelsi & best_detects$TagID == idsi)>0){
        best_detects$Subject[best_detects$Channel == channelsi & best_detects$TagID == idsi] <- k
        k <- k + 1
      }
    }
  }
  for (i in unique(flight_nums)){
    best_detects$Day[best_detects$FlightNum == times_conversion_mat[i,1]] <- times_conversion_mat[i,2]
  }
  flight_nums <- order(unique(best_detects$FlightNum))
  subjects <- order(unique(best_detects$Subject))
  for (flightsi in flight_nums[2:length(flight_nums)]){
    for (subjectsi in subjects){
      prev_dets <- best_detects[best_detects$FlightNum < flightsi & best_detects$Subject==subjectsi,]
      if (nrow(prev_dets) > 0){
        prev_lat <- prev_dets$Latitude[prev_dets$FlightNum == max(prev_dets$FlightNum)]
        prev_lon <- prev_dets$Longitude[prev_dets$FlightNum == max(prev_dets$FlightNum)]
      }else{
        prev_lat <- NA
        prev_lon <- NA
      }
      this_lat <- best_detects$Latitude[best_detects$FlightNum==flightsi & best_detects$Subject==subjectsi]
      this_lon <- best_detects$Longitude[best_detects$FlightNum==flightsi & best_detects$Subject==subjectsi]
      if (is.na(prev_lat[1]) || is.na(this_lat[1])){
        best_detects$Disp[best_detects$FlightNum==flightsi & best_detects$Subject==subjectsi] <- NA
      } else{
        best_detects$Disp[best_detects$FlightNum==flightsi & best_detects$Subject==subjectsi] <- round(pointDistance(c(prev_lon, prev_lat), c(this_lon, this_lat), lonlat=F)/1000,3)
      }
    }
  }
  r_df <- data.frame(best_detects$Channel, best_detects$TagID, best_detects$Subject, best_detects$DateTime, best_detects$Day, best_detects$Latitude, best_detects$Longitude, best_detects$Disp, best_detects$Status)
  names(r_df) <- c("Channel", "TagID", "Subject", "DateTime", "Day", "Latitude", "Longitude", "Displacement", "TagStatus")
  r_df$DisInd <- r_df$TagInd <- rep(NA, nrow(r_df))
  for(i in 1:nrow(r_df)){
    if (is.na(r_df$Displacement[i])){
      r_df$DispInd[i] <- 1
    }else if (r_df$Displacement[i] <= t_star){
      r_df$DispInd[i] <- 2
    }else{
      r_df$DispInd[i] <- 3
    }
    if (r_df$TagStatus[i] == "Mort"){
      r_df$TagInd[i] <- 2
    }else{
      r_df$TagInd[i] <- 3
    }
  }
  fish <- sort(unique(r_df$Subject))
  detect_periods <- sort(unique(r_df$Day))
  df <- as.data.frame(sfsmisc::xy.grid(fish, detect_periods))
  names(df) <- c("subject", "time")
  df$obs1 <- 1
  df$obs2 <- 1
  for (i in 1:nrow(r_df)){
    s <- r_df$Subject[i]
    d <- r_df$Day[i]
    obs1 <- r_df$DispInd[r_df$Subject==s & r_df$Day==d]
    obs2 <- r_df$TagInd[r_df$Subject==s & r_df$Day==d]
    df$obs1[df$subject==s & df$time==d] <- obs1
    df$obs2[df$subject==s & df$time==d] <- obs2
  }
  df$obs <- cbind(obs1 = df$obs1, obs2 = df$obs2)
  df$Channel <- df$TagID <- df$DateTime  <- df$Latitude <- df$Longitude <- df$Displacement <- df$TagStatus <- NA
  for (i in 1:nrow(df)){
    bool <- df$subject[i]==r_df$Subject & df$time[i]==r_df$Day
    if (sum(bool)==1){
      df$TagStatus[i] <- r_df$TagStatus[bool]
      df$Channel[i] <- r_df$Channel[bool]
      df$TagID[i] <- r_df$TagID[bool]
      df$DateTime[i] <- r_df$DateTime[bool]
      df$Latitude[i] <- r_df$Latitude[bool]
      df$Longitude[i] <- r_df$Longitude[bool]
      df$Displacement[i] <- r_df$Displacement[bool]
      df$TagStatus[i] <- r_df$TagStatus[bool]
    }
  }
  return(df)
}



