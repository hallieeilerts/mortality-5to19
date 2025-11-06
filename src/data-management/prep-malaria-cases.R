################################################################################
#' @description Load single case data, keep malaria cases
#' @return Data frame with c("iso3", "year", "cases_malaria_05to19")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
## Single causes
dat_filename <- list.files("./data/single-causes")
dat_filename <- dat_filename[grepl("wide", dat_filename, ignore.case = TRUE)] 
singlecauses <- read.csv(paste0("./data/single-causes/", dat_filename, sep = ""))
## Classification keys
key_agesexgrp <- read.csv("./gen/data-management/output/key_agesexgrp_u20.csv")
################################################################################

dat <- singlecauses[,c("iso3", "year", "AgeSexLabel", "MalariaCases")]

names(dat)[names(dat) == "MalariaCases"] <- "cases_malaria_05to19"

# Keep age/sex group of interest
dat <- subset(dat, AgeSexLabel == "Years5to19")

# Keep years of interest
dat <- subset(dat, year %in% Years)

# Tidy up
dat <- dat[,c("iso3", "year", "cases_malaria_05to19")]
dat <- dat[order(dat$iso3, dat$year), ]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/prediction/input/cases_malaria_05to19.csv", sep=""), row.names = FALSE)

