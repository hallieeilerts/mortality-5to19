################################################################################
#' @description Load single case data, keep crisis
#' @return Data frame with c("iso3", "year", and crisis columns)
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
## Single causes
dat_filename <- list.files("./data/single-causes")
dat_filename <- dat_filename[grepl("wide", dat_filename)]
singlecauses <- read.csv(paste0("./data/single-causes/", dat_filename, sep = ""))
################################################################################

# Keep age/sex group of interest
dat <- subset(singlecauses, AgeSexSuffix == ageSexSuffix)

# Keep years of interest
dat <- subset(dat, year %in% Years)

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
dat <- dat[, c("iso3", "year", "end_colvio", "epi_colvio", "end_natdis", "epi_natdis", "end_othercd", "epi_othercd", "end_diar", "epi_diar", "end_othercd_prorata", "epi_othercd_prorata")]
dat <- dat[order(dat$iso3, dat$year), ]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/squeezing/input/dat_crisis_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)
