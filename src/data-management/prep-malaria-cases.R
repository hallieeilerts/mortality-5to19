################################################################################
#' @description Load single case data, keep malaria cases
#' @return Data frame with c("ISO3", "Year", "cases_malaria_05to19")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
## Single causes
dat_filename <- list.files("./data/single-causes")
dat_filename <- dat_filename[grepl("wide", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[!grepl("india", dat_filename, ignore.case = TRUE)] 
singlecauses <- read.csv(paste0("./data/single-causes/", dat_filename, sep = ""))
## Classification keys
key_agesexgrp <- read.csv("./gen/data-management/output/key_agesexgrp_u20.csv")
################################################################################

dat <- singlecauses

# # PATCH TO WORK WITH OLD SINGLE CAUSES 20250224
# # documenting change from jags to stan
# names(dat)[which(names(dat) == "ISO3")] <- "iso3"
# names(dat)[which(names(dat) == "Year")] <- "year"
# ##########

dat <- dat[,c(idVars, names(key_agesexgrp), "MalariaCases")]
names(dat)[names(dat) == "MalariaCases"] <- "cases_malaria_05to19"

# Keep age/sex group of interest
dat <- dat[which(dat$AgeSexLabel == "Years5to19"), ]

# Keep years of interest
dat <- subset(dat, dat[[idVars[2]]] %in% Years)

# Tidy up
dat <- dat[,c(idVars, "cases_malaria_05to19")]
dat <- dat[order(dat[[idVars[1]]], dat[[idVars[2]]]), ]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/prediction/input/cases_malaria_05to19.csv", sep=""), row.names = FALSE)

