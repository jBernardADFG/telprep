# -------------------------------------------------------------------------
#' Read in txt file from telemetry flight
#'
#' @param input_file The file path of the txt file.
#' @param remove_999 Use remove_999=T to remove all 999 tag IDs. Use remove_999=F otherwise.
#' @param mort_sig When Tag IDs > 100 signal mortality, use mort_sig=T. Use mort_sig=F otherwise.
#' @param date_format The program uses POSIXct dates internally. If the date was programmed with an unusual format, it can be modified here.
#' @return Returns a data.frame representation of the raw data.


read.flight.file <- function(input_file, remove_999=T, mort_sig=T, try_formats=c("%m/%d/%y %H:%M:%OS", "%m/%d/%Y %H:%M:%OS")){
  rawdata <- scan(file=input_file, what="character", quiet=T)
  start_read <- which(rawdata=="Longitude")[1]+1
  rawdata_trunc <- rawdata[start_read:length(rawdata)]
  end_read <- which(is.element(rawdata_trunc,c("End", "ID")))[1]-1
  all_detects <- as.data.frame(matrix(rawdata_trunc[1:end_read], ncol=8, byrow=T), stringsAsFactors=F)
  names(all_detects) <- c("Date", "Time", "Channel", "TagID", "Antenna", "Power", "Latitude", "Longitude")
  all_detects$Channel <- as.numeric(all_detects$Channel)
  all_detects$TagID <- as.numeric(all_detects$TagID)
  all_detects$Power <- as.numeric(all_detects$Power)
  all_detects$Latitude <- as.numeric(all_detects$Latitude)
  all_detects$Longitude <- as.numeric(all_detects$Longitude)
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
  }else{
    if (remove_999){
      all_detects <- all_detects[all_detects$TagID != 999,]
    }
  }
  date_time <- paste(all_detects$Date, all_detects$Time)
  all_detects$DateTime <- as.POSIXct(date_time, format=try_formats[1])
  for(i in 2:length(try_formats)){
    if (is.na(all_detects$DateTime[1])){
      all_detects$DateTime <- as.POSIXct(date_time, format=try_formats[i])
    }
  }
  all_detects <- all_detects[,names(all_detects)!="Date" & names(all_detects)!="Time"]
  all_detects <- all_detects[,c(ncol(all_detects),1:(ncol(all_detects)-1))]
  all_detects <- all_detects[order(all_detects$DateTime),]
  return(all_detects)
}
