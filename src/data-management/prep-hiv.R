################################################################################
#' @description Load single case data, keep hiv
#' @return Data frame with c("iso3", "year", "HIV")
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

# Tidy up
dat <- dat[,c("iso3", "year", "HIV", "hiv_lb", "hiv_ub")]
dat <- dat[order(dat$iso3, dat$year), ]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/squeezing/input/dat_hiv_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)
