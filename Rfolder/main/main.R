# rm(list=objects())
# library(rgdal)
# library(sp)
# library(raster)
# library(riverdist)
#
# # get the location data
# folder_path <- "C:/Users/19708/Desktop/data/"
# raw_data <- read.flight.data(folder_path)
# source_vec <- c(rep(c("belly","wing"), 10), rep(c("wing","belly"),2), c("belly","wing"))
# all_data <- combine.data(raw_data, source_vec)
#
# # From yesterday
# load("D:/Jordy/telprep/telprep/data/river-net.Rdata") # loads river_net -- see river_dist package for help
# load("D:/Jordy/telprep/telprep/data/spdf.Rdata")
# crs_1 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
# crs_2 <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# river_detects <- rm.land.detects(all_data, river_net, crs_1, crs_2)
#
# spdf <- crop(spdf, extent(11433.68, 670070.35, 1428394, 1750646)) # to crop extent
# plot.locations.2(spdf, river_detects, flight_num=NA, channel=22, tag_id=21)
# plot.locations.2(spdf, river_detects, flight_num=2, channel=NA, tag_id=NA)
# plot.locations.2(spdf, river_detects, flight_num=3, channel=NA, tag_id=NA)
# plot.locations.2(spdf, river_detects, flight_num=4, channel=NA, tag_id=NA)
#
