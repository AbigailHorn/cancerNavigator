# setwd("/Volumes/GoogleDrive/Shared drives/Disparities Navigator")
# d <- read.csv("LAC_AAIRs_bySiteSex5YearRangeRaceEth_20002019_03FEB22.csv", na.strings = c("NA", "~"))

data.readin = function(){
  library(tidyverse)
  library(here)
  library(fs)
  navigator.dir = path("/Volumes/GoogleDrive/Shared drives/Disparities Navigator")
  d <- read.csv(here(navigator.dir, "LAC_AAIRs_bySiteSex5YearRangeRaceEth_20002019_03FEB22.csv"), na.strings = c("NA", "~"))
  return(d)
}

d = data.readin()