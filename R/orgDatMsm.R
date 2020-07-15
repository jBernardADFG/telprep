#' Organizes the data so that a Hidden Markov Model can be fit
#'
#' @param best_detects Output of \code{\link{get_locations}} or \code{\link{get_best_locations}}.
#' @param t_star The distance in km at which a fish is considered to have stayed in place between detection periods.
#' @return returns a data.frame that will be taken as input into \code{\link{fit_hmm}}.
#' @export
#' @examples
#' org_dat <- org_dat_msm(best_detects)

org_dat_msm <- function(best_detects, t_star=1){
  if (!requireNamespace("sfsmisc", quietly = TRUE)) {
    stop("Package \"sfsmisc\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  best_detects <- best_detects[order(best_detects$DateTime),]
  flight_nums <- best_detects$FlightNum
  med_times <- rep(NA, length(unique(flight_nums)))
  for (i in unique(flight_nums)){
    if(length(median(best_detects$DateTime[flight_nums==i]))>=1){
      med_times[i] <- median(best_detects$DateTime[flight_nums==i])
    }
  }
  med_times <- floor((med_times-min(med_times))/86400)+1
  times_conversion_mat <- cbind(unique(flight_nums), med_times)
  best_detects$Subject <- best_detects$Day <-  best_detects$Disp <- rep(NA, nrow(best_detects))
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
        prev_lat <- prev_dets$Y[prev_dets$FlightNum == max(prev_dets$FlightNum)]
        prev_lon <- prev_dets$X[prev_dets$FlightNum == max(prev_dets$FlightNum)]
      }else{
        prev_lat <- NA
        prev_lon <- NA
      }
      this_lat <- best_detects$Y[best_detects$FlightNum==flightsi & best_detects$Subject==subjectsi]
      this_lon <- best_detects$X[best_detects$FlightNum==flightsi & best_detects$Subject==subjectsi]
      if (length(prev_lat[1])==0 || length(this_lat[1])==0){
        best_detects$Disp[best_detects$FlightNum==flightsi & best_detects$Subject==subjectsi] <- NA
      } else{
        best_detects$Disp[best_detects$FlightNum==flightsi & best_detects$Subject==subjectsi] <- round( sqrt((prev_lon-this_lon)^2+(prev_lat-this_lat)^2)/1000,3)
      }
    }
  }
  r_df <- data.frame(best_detects$Channel, best_detects$TagID, best_detects$Subject, best_detects$DateTime, best_detects$Day, best_detects$Y, best_detects$X, best_detects$Disp, best_detects$Status, best_detects$FlightNum)
  names(r_df) <- c("Channel", "TagID", "Subject", "DateTime", "Day", "Y", "X", "Displacement", "TagStatus", "FlightNum")
  r_df$DisInd <- r_df$TagInd <- NA
  for(i in 1:nrow(r_df)){
    if (is.na(r_df$Displacement[i])){
      r_df$DispInd[i] <- 3
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
  df$Channel <- df$TagID <- df$DateTime  <- df$Y<- df$X <- df$Displacement <- df$TagStatus <- df$FlightNum <- NA
  for (i in 1:nrow(df)){
    bool <- df$subject[i]==r_df$Subject & df$time[i]==r_df$Day
    if (sum(bool)==1){
      df$TagStatus[i] <- r_df$TagStatus[bool]
      df$Channel[i] <- r_df$Channel[bool]
      df$TagID[i] <- r_df$TagID[bool]
      df$DateTime[i] <- r_df$DateTime[bool]
      df$Y[i] <- r_df$Y[bool]
      df$X[i] <- r_df$X[bool]
      df$Displacement[i] <- r_df$Displacement[bool]
      df$TagStatus[i] <- r_df$TagStatus[bool]
    }
  }
  for (subject in unique(df$subject)){
    df$TagID[df$subject == subject] <- na.omit(df$TagID[df$subject == subject])[1]
    df$Channel[df$subject == subject] <- na.omit(df$Channel[df$subject == subject])[1]
    df$FlightNum[df$subject == subject] <- rep(1:sum(df$subject==1))
  }
  return(df)
}



