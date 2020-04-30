# rm(list=objects())
# {
#   library(telprep)
#   directory <- "C:/Users/19708/Desktop/data/"
#   crs_data <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#   crs_sldf <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#   raw_data <- read.flight.data(directory, crs_data, crs_sldf)
#   raw_data[[2]] <- channels.merge(raw_data[[2]], 10, 3)
#   raw_data[[16]] <- replace.date(raw_data[[16]], new_date ="10/17/19")
#   raw_data[[18]] <- replace.date(raw_data[[18]], new_date ="10/7/19")
#   source_vec <- c(rep(c("belly","wing"), 10), rep(c("wing","belly"),2), c("belly","wing"))
#   all_data <- combine.data(raw_data, source_vec)
#   all_data <- all_data[all_data$TagID <= 100, ]
#   load("D:/Jordy/telprep/telprep/data/river-net.Rdata")
#   river_detects <- rm.land.detects(all_data, river_net, dist_thresh = 500)
#   flight_group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,5,5,6,6,7,7,7,7,8,8)
#   date_bins <- get.date.bins(raw_data, flight_group)
#   best_locations <- get.best.locations(river_detects, date_bins =  date_bins, bin_by=NA, n_thresh = 5, dist_max = 5, remove_flagged = T)
#   best_detects <- best_locations$best_detects
# }
# ######  To feed get msm running  ######
# msm_dat <-org.dat.msm(best_detects)



