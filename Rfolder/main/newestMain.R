rm(list=objects())
library(devtools)
devtools::install_github("jBernardADFG/telprep", build_vignette=T, force=T)
library(telprep)

setwd("D:/Jordy/telprep/telprep/data/sf")
directory <- "S:/Jordy/telprep/telprep/data/raw-data/"
crs_in <- "+proj=longlat +datum=WGS84 +no_defs"
raw_data <- read_flight_data(directory, crs_in) # Some things to touch up here ...
raw_data[[2]] <- channels_merge(raw_data[[2]], 10, 3)
raw_data[[16]] <- replace_date(raw_data[[16]], new_date ="10/17/19")
raw_data[[18]] <- replace_date(raw_data[[18]], new_date ="10/7/19")
source_vec <- c(rep(c("belly","wing"), 10), rep(c("wing","belly"),2), c("belly","wing"))
all_data <- combine_data(raw_data, source_vec)
all_data <- all_data[all_data$TagID <= 100, ]
sldf <- rgdal::readOGR("example.shp")
sldf <- sp::spTransform(sldf, attr(all_data, "crs"))
rivernet <- riverdist::line2network(sldf)
river_detects <- rm_land_detects(all_data, rivernet, dist_thresh=500)

flight_group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,5,5,6,6,7,7,7,7,8,8)
date_bins <- get_date_bins(raw_data, flight_group)
best_locations <- get_best_locations(river_detects, date_bins =  date_bins, bin_by=NA, n_thresh = 5, dist_max = 5000, remove_flagged = T)
best_detects <- best_locations$best_detects
org_dat <- org_dat_msm(best_detects)
baum_welch <- bwv(org_dat)
extract.results(baum_welch, cl=0.90)
viterbi <- viterbi(baum_welch, org_dat, best_detects)

sldf@bbox[1,1] <- 466060
sldf@bbox[1,2] <- 1174579
sldf@bbox[2,1] <- 6835662
sldf@bbox[2,2] <- 7499016
sldf <- crop(sldf, sldf@bbox)


make_plot(sldf, river_detects)
make.plot(sldf, viterbi, flight=8,open_maps=T, type="bing", darken=2.5, col_by_fish=F, viterbi=T)
best_detects <- get.locations(raw_data, flight_group, rivernet,n_d=5, d_1=1000, d_2=1000)

org.dat.msm(best_detects)







