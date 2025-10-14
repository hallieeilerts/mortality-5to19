################################################################################
#' @description Load single case data, keep crisis
#' @return Data frame with c("ISO3", "Year", "Sex", "HIV")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
## Single causes
dat_filename <- list.files("./data/single-causes")
dat_filename <- dat_filename[grepl("wide", dat_filename)] 
singlecauses <- read.csv(paste0("./data/single-causes/", dat_filename, sep = ""))
################################################################################

dat <- singlecauses

# Keep age/sex group of interest
dat <- dat[which(dat$AgeSexSuffix == ageSexSuffix), ]

# # PATCH TO WORK WITH OLD SINGLE CAUSES 20250224
# # documenting change from jags to stan
# names(dat)[which(names(dat) == "ISO3")] <- "iso3"
# names(dat)[which(names(dat) == "Year")] <- "year"
# dat$epi_colvio <- dat$colvio_epi
# dat$epi_natdis <- dat$natdis_epi
# dat$end_colvio <- dat$CollectVio
# dat$end_natdis <- dat$NatDis
# ##########

# Keep years of interest
dat <- subset(dat, dat[[idVars[2]]] %in% Years)

# There are some zeros in wide format for years where crisis deaths not reported
# Add in zeros for missing years
# This is important for how this cause is squeezed in. Otherwise, NAs are created in CSMFs.
dat$end_colvio[is.na(dat$end_colvio)] <- 0
dat$epi_colvio[is.na(dat$epi_colvio)] <- 0
dat$end_natdis[is.na(dat$end_natdis)] <- 0
dat$epi_natdis[is.na(dat$epi_natdis)] <- 0

dat$end_othercd[is.na(dat$end_othercd)] <- 0
dat$epi_othercd[is.na(dat$epi_othercd)] <- 0

dat$end_diar[is.na(dat$end_diar)] <- 0
dat$epi_diar[is.na(dat$epi_diar)] <- 0
dat$end_othercd_prorata[is.na(dat$end_othercd_prorata)] <- 0
dat$epi_othercd_prorata[is.na(dat$epi_othercd_prorata)] <- 0


# Tidy up
#dat <- dat[, c(idVars[1:2],  "end_colvio", "epi_colvio", "end_natdis", "epi_natdis")]
dat <- dat[, c(idVars, "end_colvio", "epi_colvio", "end_natdis", "epi_natdis", "end_othercd", "epi_othercd", "end_diar", "epi_diar", "end_othercd_prorata", "epi_othercd_prorata")]
dat <- dat[order(dat[[idVars[1]]], dat[[idVars[2]]]), ]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/squeezing/input/dat_crisis_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)
