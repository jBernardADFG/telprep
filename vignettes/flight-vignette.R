## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(telprep)
library(rlist)

## ----eval=FALSE----------------------------------------------------------
#  folder_path <- "D:/Jordy/telprep/telprep2/data/" # Use your folder path here
#  raw_data <- read.flight.data(folder_path, remove_999 = T, mort_sig = T)

## ----include=FALSE-------------------------------------------------------
load("raw_data")

