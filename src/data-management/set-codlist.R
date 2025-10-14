################################################################################
#' @description Set list of modeled and reported CODs for each age group
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(readxl)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("codlist", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_codlist <- read_excel(paste0("./data/keys/", dat_filename, sep = ""))
################################################################################

dat <- subset(key_codlist, AgeSexSuffix == ageSexSuffix)

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste0("./gen/data-management/output/key_codlist_", ageSexSuffix, ".csv"), row.names = FALSE)
