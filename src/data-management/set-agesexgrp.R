################################################################################
#' @description Set standard columns for age sex groups
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("agesexgroups", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_agesexgrp  <- read.csv(paste0("./data/keys/", dat_filename, sep = ""))
################################################################################

# Save output(s) ----------------------------------------------------------

write.csv(key_agesexgrp, "./gen/data-management/output/key_agesexgrp_u20.csv", row.names = FALSE)
