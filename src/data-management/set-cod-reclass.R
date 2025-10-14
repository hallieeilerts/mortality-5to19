################################################################################
#' @description Set cod reclassification key
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/keys/", dat_filename, sep = ""))
################################################################################

# Save output(s) ----------------------------------------------------------

write.csv(key_cod, paste0("./gen/data-management/output/key_cod_", ageSexSuffix, ".csv"), row.names = FALSE)
