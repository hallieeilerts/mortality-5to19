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
source("./src/prepare-session/create-session-variables.R")
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("countrymodelclass", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_ctryclass_u20  <- read_excel(paste0("./data/keys/", dat_filename, sep = ""), sheet = "CountryModelClass")
################################################################################

# Add code that assigns countries to GOODVR, LMM or HMM
# env_crisisfree_u20_igme <- read_excel("./data/envelopes/UN IGME 2022 Rates & Deaths_Country Summary (crisis free) 1980-2021 all ages.xlsx")
# Should we be using crisis-free or crisis-included u5m rate in 2010 to set country class?
# Need to load information on whether its a fragile state. Where does this come from?

# As a place holder, I will just re-save 20201001-CountryModelClass.csv
# and use the updated name for this object (key_ctryclass_u20)
names(key_ctryclass_u20)[which(names(key_ctryclass_u20) == "ISO3")] <- idVars[1]

# Save output(s) ----------------------------------------------------------

write.csv(key_ctryclass_u20, "./gen/data-management/output/key_ctryclass_u20.csv", row.names = FALSE)
