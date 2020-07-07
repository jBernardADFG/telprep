#
# # -------------------------------------------------------------------------
# # main
# library(rgdal)
# library(sp)
# library(raster)
# library(riverdist)
# library(telprep)
#
# # Read in shapefiles
# setwd("D:/Jordy/telprep/telprep/data/sf")
# g <- readOGR("George_Healy_Sand.shp")
# k <- readOGR("Koyukuk_Nowitna.shp")
# bad_crs <- proj4string(k)
# k <- spTransform(k, proj4string(g)) # Transform coordinate projection
# t <- readOGR("Tanana_Yukon.shp")
#
# plot(t)
# str(t)
# # How's everything organized?
# plot(g, xlim=c(-175152, 479040), ylim=c(1505719, 1902073)) # can ignore
# plot(t, add=T, col="red")
# plot(k, add=T, col="blue")
#
# # add relevant info into a single SLDF
# sl <- aggregate(rbind(k, t))
# spdf <- SpatialLinesDataFrame(sl, data.frame(Z = c("lines")))
# plot(spdf)
#
# # use riverdist to set up the river network
# r_net <- line2network(sp=spdf, tolerance = 500)
# plot(r_net)
# crn <- cleanup(r_net)
# plot(crn)
#
# river_net <- crn
# choose.dir()
# save(river_net, file="D:/Jordy/telprep/telprep/data/river-net.Rdata")
# save(spdf, file="D:/Jordy/telprep/telprep/data/spdf.Rdata")
#
# # -------------------------------------------------------------------------
#
# # get the location data
# folder_path <- "C:/Users/19708/Desktop/data/"
# raw_data <- read.flight.data(folder_path)
# source_vec <- c(rep(c("belly","wing"), 10), rep(c("wing","belly"),2), c("belly","wing"))
# all_data <- combine.data(raw_data, source_vec)
# load("D:/Jordy/telprep/telprep/data/river-net.Rdata") # See river_dist package for help
# crs_1 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
# crs_2 <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# all_data <- rm.land.detects(all_data, r_net, crs_1, crs_2)
