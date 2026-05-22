################################################################################
#' @description Prepare crisis-free and crisis-included envelopes
#' @return One file with envelopes for all ages and one age-specific envelope file
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
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
key_agesexgrp <- read.csv("./gen/data-management/output/key_agesexgrp_u20.csv")
################################################################################

dat <- singlecauses[,c("iso3", "year", names(key_agesexgrp), "Deaths1", "Deaths2", "Rate1", "Rate2")]
dat <- subset(dat, AgeSexSuffix %in% c("05to09y", "10to14y", "15to19yF", "15to19yM", "15to19y"))

# Keep age/sex group of interest
dat_agespecific <- dat[which(dat$AgeSexSuffix == ageSexSuffix), ]

# Keep years of interest
dat <- subset(dat, year %in% Years)

# Fill NA's with zero
dat[is.na(dat)] <- 0

# Select countries of interest
dat <- dat[which(dat$iso3 %in% unique(key_ctryclass_u20$iso3)), ]
dat_agespecific <- dat_agespecific[which(dat_agespecific$iso3 %in% unique(key_ctryclass_u20$iso3)), ]

# Tidy up
dat <- dat[, c("AgeSexSuffix","iso3", "year", "Deaths1", "Deaths2", "Rate1", "Rate2")]
dat_agespecific <- dat_agespecific[, c("iso3", "year", "Deaths1", "Deaths2", "Rate1", "Rate2")]
rownames(dat) <- NULL
rownames(dat_agespecific) <- NULL

# Save output(s) ----------------------------------------------------------

# These envelopes used for prediction database
write.csv(dat, paste("./gen/data-management/output/env_u20.csv", sep = ""), row.names = FALSE)
# These envelopes are age/sex-specific and used in all other cases
write.csv(dat_agespecific, paste("./gen/data-management/output/env_", ageSexSuffix,".csv", sep = ""), row.names = FALSE)
