################################################################################
#' @description Formats VR data for CSMF calculation.
#' @return Data frame with VR data with updated variable names.
################################################################################
#' Libraries
library(readstata13)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_filename <- list.files("./data/vr")
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1)
dat  <- read.csv(paste0("./data/vr/", dat_filename, sep = ""))
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# Rename variables
names(dat)[names(dat) == "ISO3"] <- "iso3"
names(dat)[names(dat) == "Year"] <- "year"

# keep age group of interest
dat <- subset(dat, AgeSexSuffix %in% ageSexSuffix)

# Delete columns that are added later
dat <- dat[,!names(dat) %in% c("AgeGroup", "Sex", "AgeSexSuffix")]

# Check that all expected countries are included --------------------------

if(sum(!(unique(subset(key_ctryclass_u20, Group == "VR")$iso3) %in% dat$iso3)) > 0){
  warning("Required countries missing from data input.")
  unique(subset(key_ctryclass_u20, Group == "VR")$iso3)[!(unique(subset(key_ctryclass_u20, Group == "VR")$iso3) %in% dat$iso3)]
}

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/data-management/output/csmf_vr_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)
