# # To get everything loaded
#
# library(rlist)
# library(rgdal)
# library(sp)
# library(raster)
# library(riverdist)
# setwd("D:/Jordy/telprep/telprep/data/sf")
# sldf <- readOGR("example.shp")
# directory <- "C:/Users/19708/Desktop/data/"
# crs_data <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
# crs_sldf <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# raw_data <- read.flight.data(directory, crs_data, crs_sldf)
# raw_data[[2]] <- channels.merge(raw_data[[2]], 10, 3)
# raw_data[[16]] <- replace.date(raw_data[[16]], new_date ="10/17/19")
# raw_data[[18]] <- replace.date(raw_data[[18]], new_date ="10/7/19")
# all_data <- combine.data(raw_data, source_vec)
# all_data <- all_data[all_data$TagID <= 100, ]
# load("D:/Jordy/telprep/telprep/data/river-net.Rdata")
# river_detects <- rm.land.detects(all_data, river_net, dist_thresh = 500)
# flight_group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,5,5,6,6,7,7,7,7,8,8)
# date_bins <- get.date.bins(raw_data, flight_group)
# best_locations <- get.best.locations(river_detects, date_bins =  date_bins, bin_by=NA, n_thresh = 5, dist_max = 5, remove_flagged = T)
#
#
# ########################################################
# best_detects <- best_locations$best_detects
# best_detects <- best_detects[,c(1,2,3,5,6,7,9)]
# best_detects <- best_detects[,c(1,7,2,3,4,5,6)]
# msm_data <- org.dat.msm(best_detects)
#
# dat <- msm_data$dat
#
# ## Fitted model should approximately recover true parameters
# bwv <- function(msm_data){
#
#   x <- msm(obs ~ time, subject=subject, data=msm_data, qmatrix=two.q,
#            hmodel = list(hmmBinom(size=3, prob=0.2),
#                          hmmBinom(size=2, prob=0.2)))
#   viterbi.msm(x)
# }
#
#
#
#
#
#
#
# fish <- sort(unique(msm_data$Subject))
# detect_periods <- sort(unique(msm_data$Day))
# library(sfsmisc)
# df <- as.data.frame(xy.grid(fish, detect_periods))
# names(df) <- c("subject", "time")
# df$obs1 <- 1
# df$obs2 <- 1
#
# for (i in 1:nrow(msm_data)){
#   s <- msm_data$Subject[i]
#   d <- msm_data$Day[i]
#   if(!is.na(msm_data$Displacement[msm_data$Subject==s & msm_data$Day==d])){
#     obs1 <- msm_data$Displacement[msm_data$Subject==s & msm_data$Day==d]
#   }
#   if(!is.na(msm_data$TagStatus[msm_data$Subject==s & msm_data$Day==d])){
#     obs2 <- msm_data$TagStatus[msm_data$Subject==s & msm_data$Day==d]
#   }
#   df$obs1[df$subject==s & df$time==d] <- obs1
#   df$obs2[df$subject==s & df$time==d] <- obs2
# }
#
# df$obs <- cbind(obs1 = df$obs1, obs2 = df$obs2)
#
# dat <- df

