################################################################################
#' @description Set country class from simple update 2021. used in visualizations comparison to know whether model shifted classes this round.
#' @return Data frame with c("ISO3", "Group", "Group2010", "FragileState")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
key_ctryclass_u20  <- read.csv("./data/keys/CountryModelClass_20201001.csv")
################################################################################

# Add code that assigns countries to GOODVR, LMM or HMM
# env_crisisfree_u20_igme <- read_excel("./data/envelopes/UN IGME 2022 Rates & Deaths_Country Summary (crisis free) 1980-2021 all ages.xlsx")
# Should we be using crisis-free or crisis-included u5m rate in 2010 to set country class?
# Need to load information on whether its a fragile state. Where does this come from?

# As a place holder, I will just re-save 20201001-CountryModelClass.csv
# and use the updated name for this object (key_ctryclass_u20)
names(key_ctryclass_u20)[which(names(key_ctryclass_u20) == "ISO3")] <- idVars[1]

# Save output(s) ----------------------------------------------------------

write.csv(key_ctryclass_u20, "./gen/data-management/output/key_ctryclassOld_u20.csv", row.names = FALSE)
