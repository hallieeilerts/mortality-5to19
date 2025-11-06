################################################################################
#' @description Set country class based on U5M
#' @return Data frame with c("ISO3", "Group", "Group2010", "FragileState")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(readxl)
#' Inputs
source("./src/prepare-session/set-inputs.R")
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("countrymodelclass", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_ctryclass_u20  <- read_excel(paste0("./data/keys/", dat_filename, sep = ""), sheet = "CountryModelClass")
################################################################################

names(key_ctryclass_u20)[which(names(key_ctryclass_u20) == "ISO3")] <- "iso3"

# Save output(s) ----------------------------------------------------------

write.csv(key_ctryclass_u20, "./gen/data-management/output/key_ctryclass_u20.csv", row.names = FALSE)
