################################################################################
#' @description Formats prediction database and adds mortality rate covariates so that variable names match covariates used in model estimation.
#' @return Data frame with all covariates required for prediction database
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
## Prediction Database
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("wide", dat_filename, ignore.case = TRUE)]
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
dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars[1:2])]))]

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$iso3) %in% dat$iso3)) > 0){
  stop("Required countries missing from data input.")
}

# Save output(s) ----------------------------------------------------------

write.csv(dat, "./gen/data-management/output/dat_pred_u20.csv", row.names = FALSE)
