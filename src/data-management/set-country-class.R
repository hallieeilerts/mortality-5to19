################################################################################
#' @description Set country class based on U5M
#' @return Data frame with ISO3 and VR/HMM/LMM country grouping
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(readxl)
#' Inputs
source("./src/prepare-session/set-inputs.R")
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("countrymodelclass", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1)
key_ctryclass_u20  <- read_excel(paste0("./data/keys/", dat_filename, sep = ""), sheet = "CountryModelClass")
################################################################################

# check which countries changed categories
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("countrymodelclass", dat_filename, ignore.case = TRUE)]
dat_filename <- head(sort(dat_filename),1)
key_old  <- read.csv(paste0("./data/keys/", dat_filename, sep = ""))
keyboth <- merge(key_ctryclass_u20, key_old, by = "ISO3", suffixes = c("_new", "_old"))
subset(keyboth, Group2010_old != Group2010_new)
# Went from VR to LMM:
# Barbados
# El Salvador
# Venezuela

names(key_ctryclass_u20)[which(names(key_ctryclass_u20) == "ISO3")] <- "iso3"

# Save output(s) ----------------------------------------------------------

write.csv(key_ctryclass_u20, "./gen/data-management/output/key_ctryclass_u20.csv", row.names = FALSE)
