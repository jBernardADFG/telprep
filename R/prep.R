# -------------------------------------------------------------------------
# Functions to read in and prepare raw data
# -------------------------------------------------------------------------
# Functions need to be debugged ...

# -------------------------------------------------------------------------
#' Read in and process raw txt files from telemetry flights and return the contents for an in R workflow
#'
#' @param input_file String specifying the file path of the txt file to process (eg input_file="S:/Jordy/data/input-file-name.txt).
#' @param remove_999 Logical argument. Use remove_999=T to remove all 999 tag IDs. Use remove_999=F otherwise.
#' @param mort_sig Logical argument to specify whether of not the tags give a mortality signal. When a Tag IDs > 100 signals mortality, use mort_sig=T. Use mort_sig=F otherwise.
#' @return Returns a list containing two data frames ($all_locs contains all of the detections while $best_locs contains the detections with the highest signal only).
#' @examples
#' input_file <- "S:/Jordy/telprep/data/laura/Tanana Burbot flight Dec 2019 - BELLY.txt"
#' prep.flight.df(input_file)
#' @export

prep.flight.df <- function(input_file, remove_999=T, mort_sig=T){
  print("Be patient -- this could take a minute")
  rawdata <- scan(file=input_file, what="character", quiet=T)
  start_read <- which(rawdata=="Longitude")[1]+1
  rawdata_trunc <- rawdata[start_read:length(rawdata)]
  end_read <- which(is.element(rawdata_trunc,c("End", "ID")))[1]-1
  all_locs <- as.data.frame(matrix(rawdata_trunc[1:end_read], ncol=8, byrow=T), stringsAsFactors=F)
  names(all_locs) <- c("Date", "Time", "Channel", "TagID", "Antenna", "Power", "Latitude", "Longitude")
  all_locs$Channel <- as.numeric(all_locs$Channel)
  all_locs$TagID <- as.numeric(all_locs$TagID)
  all_locs$Power <- as.numeric(all_locs$Power)
  all_locs$Latitude <- as.numeric(all_locs$Latitude)
  all_locs$Longitude <- as.numeric(all_locs$Longitude)
  if (mort_sig){

    if (remove_999){

      all_locs <- all_locs[all_locs$TagID != 999,]

      all_locs$Status <- as.factor(ifelse(all_locs$TagID > 100, "Mort","Active"))

      all_locs$TagID[all_locs$TagID>100] <- all_locs$TagID[all_locs$TagID>100]-100

    }else{

      all_locs$Status <- ifelse(all_locs$TagID > 100, "Mort","Active")
      all_locs$Status[all_locs$TagID==999] <- "999"
      all_locs$Status <- as.factor(all_locs$Status)
      all_locs$TagID[all_locs$TagID>100 & all_locs$TagID!=999] <- all_locs$TagID[all_locs$TagID>100 & all_locs$TagID!=999]-100
    }
  }
  all_locs$BestSignal <- F
  alldates <- sort(unique(all_locs$Date))
  allchannels <- sort(unique(all_locs$Channel))
  alltags <- sort(unique(all_locs$TagID))
  all_locs$n_records <-  NA
  for(datesi in alldates) {
    for(channelsi in allchannels) {
      for(tagsi in alltags) {
        if(sum(all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi) > 0) {
          maxpower <- max(all_locs$Power[all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi])
          all_locs$BestSignal[all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi & all_locs$Power==maxpower][1] <- T
          all_locs$n_records[all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi] <-
            sum(all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi)
          if(!remove_999){
            all_locs$n_records[all_locs$TagID==999] <- NA
          }
        }
      }
    }
  }
  locs <- all_locs[all_locs$TagID!=999,]
  best_locs <- subset(locs, BestSignal)
  best_locs$BestSignal <- NULL
  return(list(all_locs=all_locs, best_locs=best_locs))
}


# -------------------------------------------------------------------------
#' Read in and process raw txt files from telemetry flights and write the contents to two files in the output directory
#'
#' @param input_file String specifying the file path of the txt file to process (eg input_file="S:/Jordy/data/input-file-name.txt).
#' @param output_directory String specifying the dirctory to write the output files (eg directory="S:/Jordy/data/). Two files will be written to this directory: all_locs contains all of the detections while best_locs contains the detections with the highest signal only.
#' @param remove_999 Logical argument. Use remove_999=T to remove all 999 tag IDs. Use remove_999=F otherwise.
#' @param mort_sig #' @param mort_sig Logical argument to specify whether of not the tags give a mortality signal. When a Tag IDs > 100 signals mortality, use mort_sig=T. Use mort_sig=F otherwise.
#' @export

prep.flight.file <- function(input_file, output_directory, remove_999=T, mort_sig=T){
  print("Be patient -- this could take a minute")
  rawdata <- scan(file=input_file, what="character", quiet=T)
  start_read <- which(rawdata=="Longitude")[1]+1
  rawdata_trunc <- rawdata[start_read:length(rawdata)]
  end_read <- which(is.element(rawdata_trunc,c("End", "ID")))[1]-1
  all_locs <- as.data.frame(matrix(rawdata_trunc[1:end_read], ncol=8, byrow=T), stringsAsFactors=F)
  names(all_locs) <- c("Date", "Time", "Channel", "TagID", "Antenna", "Power", "Latitude", "Longitude")
  all_locs$Channel <- as.numeric(all_locs$Channel)
  all_locs$TagID <- as.numeric(all_locs$TagID)
  all_locs$Power <- as.numeric(all_locs$Power)
  all_locs$Latitude <- as.numeric(all_locs$Latitude)
  all_locs$Longitude <- as.numeric(all_locs$Longitude)
  if (mort_sig){
    if (remove_999){
      all_locs <- all_locs[all_locs$TagID != 999,]
      all_locs$Status <- as.factor(ifelse(all_locs$TagID > 100, "Mort","Active"))
      all_locs$TagID[all_locs$TagID>100] <- all_locs$TagID[all_locs$TagID>100]-100
    }else{
      all_locs$Status <- ifelse(all_locs$TagID > 100, "Mort","Active")
      all_locs$Status[all_locs$TagID==999] <- "999"
      all_locs$Status <- as.factor(all_locs$Status)
      all_locs$TagID[all_locs$TagID>100 & all_locs$TagID!=999] <- all_locs$TagID[all_locs$TagID>100 & all_locs$TagID!=999]-100
    }
  }
  all_locs$BestSignal <- F
  alldates <- sort(unique(all_locs$Date))
  allchannels <- sort(unique(all_locs$Channel))
  alltags <- sort(unique(all_locs$TagID))
  all_locs$n_records <-  NA
  for(datesi in alldates) {
    for(channelsi in allchannels) {
      for(tagsi in alltags) {
        if(sum(all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi) > 0) {
          maxpower <- max(all_locs$Power[all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi])
          all_locs$BestSignal[all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi & all_locs$Power==maxpower][1] <- T
          all_locs$n_records[all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi] <-
            sum(all_locs$Date==datesi & all_locs$Channel==channelsi & all_locs$TagID==tagsi)
          if(!remove_999){
            all_locs$n_records[all_locs$TagID==999] <- NA
          }
        }
      }
    }
  }
  locs <- all_locs[all_locs$TagID!=999,]
  best_locs <- subset(locs, BestSignal)
  best_locs$BestSignal <- NULL
  write.csv(all_locs, file=paste0(output_directory,"all_locs.csv"), row.names=F)
  write.csv(best_locs, file=paste0(output_directory,"best_locs.csv"), row.names=F)
}


# -------------------------------------------------------------------------
#' Read in and process raw txt files from stationary telemetry projects and return the contents for an in R workflow
#'
#' @param input_file String specifying the file path of the txt file to process (eg input_file="S:/Jordy/data/input-file-name.txt).
#' @param remove_999 Logical argument. Use remove_999=T to remove all 999 tag IDs. Use remove_999=F otherwise. Defaults to T.
#' @param mort_sig #' @param mort_sig Logical argument to specify whether of not the tags give a mortality signal. When a Tag IDs > 100 signals mortality, use mort_sig=T. Use mort_sig=F otherwise.
#' @return Returns a list containing two data frames ($all_detections contains all of the detections while $best_detections contains the detections with the highest signal only).
#' @export

prep.stationary.df <- function(input_file, remove_999=T, mort_sig=T){
  print("Be patient -- this could take a minute")
  rawdata <- scan(file=input_file, what="character", quiet=T)
  start_read <- which(rawdata=="Longitude")[1]+1
  rawdata_trunc <- rawdata[start_read:length(rawdata)]
  end_read <- which(is.element(rawdata_trunc,c("End", "ID")))[1]-1
  all_detects <- as.data.frame(matrix(rawdata_trunc[1:end_read], ncol=8, byrow=T), stringsAsFactors=F)
  names(all_detects) <- c("Date", "Time", "Channel", "TagID", "Antenna", "Power", "Duration", "Events")
  all_detects$Channel <- as.numeric(all_detects$Channel)
  all_detects$TagID <- as.numeric(all_detects$TagID)
  all_detects$Power <- as.numeric(all_detects$Power)
  all_detects$Duration <- as.numeric(all_detects$Duration)
  all_detects$Events <- as.numeric(all_detects$Events)
  if (mort_sig){
    if (remove_999){
      all_detects <- all_detects[all_detects$TagID != 999,]
      all_detects$Status <- as.factor(ifelse(all_detects$TagID > 100, "Mort","Active"))
      all_detects$TagID[all_detects$TagID>100] <- all_detects$TagID[all_detects$TagID>100]-100
    }else{
      all_detects$Status <- ifelse(all_detects$TagID > 100, "Mort","Active")
      all_detects$Status[all_detects$TagID==999] <- "999"
      all_detects$Status <- as.factor(all_detects$Status)
      all_detects$TagID[all_detects$TagID>100 & all_detects$TagID!=999] <- all_detects$TagID[all_detects$TagID>100 & all_detects$TagID!=999]-100
    }
  }
  all_detects$BestSignal <- F
  alldates <- sort(unique(all_detects$Date))
  allchannels <- sort(unique(all_detects$Channel))
  alltags <- sort(unique(all_detects$TagID))
  all_detects$n_records <-  NA
  for(datesi in alldates) {
    for(channelsi in allchannels) {
      for(tagsi in alltags) {
        if(sum(all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi) > 0) {
          maxpower <- max(all_detects$Power[all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi])
          all_detects$BestSignal[all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi & all_detects$Power==maxpower][1] <- T
          all_detects$n_records[all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi] <-
            sum(all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi)
          if(!remove_999){
            all_detects$n_records[all_detects$TagID==999] <- NA
          }
        }
      }
    }
  }
  detects <- all_detects[all_detects$TagID!=999,]
  best_detects <- subset(detects, BestSignal)
  best_detects$BestSignal <- NULL
  return(list(all_detects=all_detects, best_detects=best_detects))
}


# -------------------------------------------------------------------------
#' Read in and process raw txt files from stationary telemetry projects and write the contents to two files in the output directory
#'
#' @param input_file String specifying the file path of the txt file to process (eg input_file="S:/Jordy/data/input-file-name.txt).
#' @param output_directory String specifying the dirctory to write the output files (eg directory="S:/Jordy/data/). Two files will be written to this directory. The first file (all_detections) lists all of the detections whereas the second file (best_detections) lists the detections with the highest signal. To supress the file output, use output_directory=NA.
#' @param remove_999 Logical argument. Use remove_999=T to remove all 999 tag IDs. Use remove_999=F otherwise.
#' @param mort_sig #' @param mort_sig Logical argument to specify whether of not the tags give a mortality signal. When a Tag IDs > 100 signals mortality, use mort_sig=T. Use mort_sig=F otherwise.
#' @export

prep.stationary.file <- function(input_file, output_directory, remove_999=T, mort_sig=F, return=F){
  print("Be patient -- this could take a minute")
  rawdata <- scan(file=input_file, what="character", quiet=T)
  start_read <- which(rawdata=="Longitude")[1]+1
  rawdata_trunc <- rawdata[start_read:length(rawdata)]
  end_read <- which(is.element(rawdata_trunc,c("End", "ID")))[1]-1
  all_detects <- as.data.frame(matrix(rawdata_trunc[1:end_read], ncol=8, byrow=T), stringsAsFactors=F)
  names(all_detects) <- c("Date", "Time", "Channel", "TagID", "Antenna", "Power", "Duration", "Events")
  all_detects$Channel <- as.numeric(all_detects$Channel)
  all_detects$TagID <- as.numeric(all_detects$TagID)
  all_detects$Power <- as.numeric(all_detects$Power)
  all_detects$Duration <- as.numeric(all_detects$Duration)
  all_detects$Events <- as.numeric(all_detects$Events)
  if (mort_sig){
    if (remove_999){
      all_detects <- all_detects[all_detects$TagID != 999,]
      all_detects$Status <- as.factor(ifelse(all_detects$TagID > 100, "Mort","Active"))
      all_detects$TagID[all_detects$TagID>100] <- all_detects$TagID[all_detects$TagID>100]-100
    }else{
      all_detects$Status <- ifelse(all_detects$TagID > 100, "Mort","Active")
      all_detects$Status[all_detects$TagID==999] <- "999"
      all_detects$Status <- as.factor(all_detects$Status)
      all_detects$TagID[all_detects$TagID>100 & all_detects$TagID!=999] <- all_detects$TagID[all_detects$TagID>100 & all_detects$TagID!=999]-100
    }
  }
  all_detects$BestSignal <- F
  alldates <- sort(unique(all_detects$Date))
  allchannels <- sort(unique(all_detects$Channel))
  alltags <- sort(unique(all_detects$TagID))
  all_detects$n_records <-  NA
  for(datesi in alldates) {
    for(channelsi in allchannels) {
      for(tagsi in alltags) {
        if(sum(all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi) > 0) {
          maxpower <- max(all_detects$Power[all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi])
          all_detects$BestSignal[all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi & all_detects$Power==maxpower][1] <- T
          all_detects$n_records[all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi] <-
            sum(all_detects$Date==datesi & all_detects$Channel==channelsi & all_detects$TagID==tagsi)
          if(!remove_999){
            all_detects$n_records[all_detects$TagID==999] <- NA
          }
        }
      }
    }
  }
  detects <- all_detects[all_detects$TagID!=999,]
  best_detects <- subset(detects, BestSignal)
  best_detects$BestSignal <- NULL
  write.csv(all_detects, file=paste0(output_directory,"all_detects.csv"), row.names=F)
  write.csv(best_detects, file=paste0(output_directory,"best_detects.csv"), row.names=F)
}







