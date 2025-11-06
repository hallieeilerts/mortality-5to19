################################################################################
#' @description Adds variables for prediction, subsets data for countries/years of interest
#' @return Data frame used for prediction
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
## Prediction Database
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("ext-wide", dat_filename, ignore.case = TRUE)]
dat_pred <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

dat <- dat_pred

# add variables needed in prediction pipeline
dat$pid <- 1:nrow(dat)
dat$intercept <- 1

# limit to years of interest
dat <- dat[dat$year %in% Years, ]
rownames(dat) <- NULL

# Tidy up
dat <- dat[, c("iso3", "year", sort(names(dat)[which(!names(dat) %in% c("iso3", "year"))]))]

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$iso3) %in% dat$iso3)) > 0){
  stop("Required countries missing from data input.")
}

# Save output(s) ----------------------------------------------------------

write.csv(dat, "./gen/data-management/output/dat_pred_u20.csv", row.names = FALSE)
