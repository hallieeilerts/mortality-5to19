################################################################################
#' @description Load single case data, keep hiv
#' @return Data frame with c("ISO3", "Year", "Sex", "HIV")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
## Single causes
dat_filename <- list.files("./data/single-causes")
dat_filename <- dat_filename[grepl("wide", dat_filename)] 
singlecauses <- read.csv(paste0("./data/single-causes/", dat_filename, sep = ""))
################################################################################

dat <- singlecauses

# # PATCH TO WORK WITH OLD SINGLE CAUSES 20250224
# # documenting change from jags to stan
# names(dat)[which(names(dat) == "ISO3")] <- "iso3"
# names(dat)[which(names(dat) == "Year")] <- "year"
# ##########

# Keep age/sex group of interest
dat <- dat[which(dat$AgeSexSuffix == ageSexSuffix), ]

# Keep years of interest
dat <- subset(dat, dat[[idVars[2]]] %in% Years)

# Tidy up
dat <- dat[,c(idVars, "HIV")]
dat <- dat[order(dat[[idVars[1]]], dat[[idVars[2]]]), ]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/squeezing/input/dat_hiv_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)
