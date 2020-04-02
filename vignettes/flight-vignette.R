## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(telprep)
library(rlist)

## ------------------------------------------------------------------------
folder_path <- "D:/Jordy/telprep/telprep2/data/" # Use your folder path here
raw_data <- read.flight.data(folder_path, remove_999 = T, mort_sig = T)

## ------------------------------------------------------------------------
head(names(raw_data))
head(raw_data[[2]])

## ------------------------------------------------------------------------
raw_data[[2]] <- channels.merge(raw_data[[2]], 10, 3) # see help(merge_channels)
head(raw_data[[2]])

## ------------------------------------------------------------------------
head(raw_data[[16]]) # 2003 ?
raw_data[[16]] <- replace.date(raw_data[[16]], new_date ="04/25/19") # see help(replace.date)?
head(raw_data[[16]])

## ------------------------------------------------------------------------
head(names(raw_data)) # to determine source_vec
source_vec <- c(rep(c("belly","wing"), 10), rep(c("wing","belly"),2), c("belly","wing"))
all_data <- combine.data(raw_data, source_vec) # See help(combine.data)

## ----message=FALSE-------------------------------------------------------
best_locations <- get.best.locations(all_data, bin_by = 1, n_thresh = 5, dist_max = 10, remove_flagged = F) # See help(process_data)
all_detects <- best_locations$all_detects
best_detects <- best_locations$best_detects

## ------------------------------------------------------------------------
head(all_detects)
head(best_detects)

## ------------------------------------------------------------------------
best_detects <- best_detects[!best_detects$flag,] # To remove the flagged fish
flagged_fish <- flag.dead.fish(best_detects, dist_thresh = 5) # To flag for mortality
head(flagged_fish)

## ------------------------------------------------------------------------
# filepath <- "D:/Jordy/telprep/telprep2/data/database.csv"
# add.to.database(filepath, flagged_fish)

