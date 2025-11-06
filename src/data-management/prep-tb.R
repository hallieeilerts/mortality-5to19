################################################################################
#' @description Load single case data, keep tb
#' @return Data frame with c("ISO3", "Year", "Sex", "HIV")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
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

# Tidy up
dat <- dat[, c("iso3", "year", "TB", "tb_lb", "tb_ub", "TBre", "tbre_lb", "tbre_ub")]
dat <- dat[order(dat$iso3, dat$year), ]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/squeezing/input/dat_tb_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)
