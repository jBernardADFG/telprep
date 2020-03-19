#' Function to combine the contents of multiple "best location" or "best detection" data.frames (eg the "belly" and "wing" detections from a single flight).
#'
#' @param df_list A list containing the data.frames to merge. The column names of each data frame in the list need to be the same.
#' @param source_vec A vector of strings to assigning a unique identifier to each data.frame or file. The ordering of df_list and source_vec should coorespond (eg: to merge list(belly_df, wing_df) use source_vec=c("belly", "wing")).
#' @return Returns a data.frame where the rows coorespond to the "best location" or "best detection" when the data is pooled
#' @examples
#' belly_file <- "S:/Jordy/telprep/data/laura/Tanana Burbot flight Dec 2019 - BELLY.txt"
#' wing_file <- "S:/Jordy/telprep/data/laura/Tanana Burbot flight Dec 2019 - Wing.txt"
#' df_list <- list(prep.flight.df(belly_file)$best_loc, prep.flight.df(wing_file)$best_loc)
#' source_vec <- c("Belly", "Wing")
#' combine.best.detections.df(df_list, source_vec)
#' @export

combine.best.detections.df <- function(df_list, source_vec){
  comb_df <- data.frame()
  for (i in 1:length(source_vec)){
    df_list[[i]]$Source <- source_vec[i]
    comb_df <- rbind(comb_df, df_list[[i]])
  }
  alldates <- sort(unique(comb_df$Date))
  allchannels <- sort(unique(comb_df$Channel))
  alltags <- sort(unique(comb_df$TagID))
  r_df <- data.frame()
  for(datesi in alldates){
    for(channelsi in allchannels){
      for(tagsi in alltags){
        detect_df <- comb_df[comb_df$Date==datesi & comb_df$Channel==channelsi & comb_df$TagID==tagsi,]
        if (nrow(detect_df)!=0){
          new_row <- detect_df[detect_df$Power==max(detect_df$Power),]
          new_row$n_records <- sum(detect_df$n_records)
          new_row[,(ncol(new_row)+1):(ncol(new_row)+length(source_vec))] <- 0
          name_vec <- paste("n_", source_vec, sep="")
          names(new_row)[(ncol(new_row)-length(source_vec)+1):ncol(new_row)] <- name_vec
          for (i in 1:length(source_vec)){
            if (length(detect_df$n_records[detect_df$Source==source_vec[i]])!=0){
              n <- detect_df$n_records[detect_df$Source==source_vec[i]]
              if (length(n)!=0){
                new_row[1,ncol(detect_df)+i] <- n
                r_df <- rbind(r_df, new_row)
              }
            }
          }
        }
      }
    }
  }
  return(r_df)
}

# combine.best.detections.file <- function(filename_vec, source_vec){
#
# }

x <- 1










