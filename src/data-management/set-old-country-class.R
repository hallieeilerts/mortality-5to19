################################################################################
#' @description Set country class from simple update 2021. used in visualizations comparison to know whether model shifted classes this round.
#' @return Data frame with c("ISO3", "Group", "Group2010", "FragileState")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
key_ctryclass_u20  <- read.csv("./data/keys/CountryModelClass_20201001_SimpleUpdate2021.csv")
################################################################################

names(key_ctryclass_u20)[which(names(key_ctryclass_u20) == "ISO3")] <- "iso3"

# Save output(s) ----------------------------------------------------------

write.csv(key_ctryclass_u20, "./gen/data-management/output/key_ctryclassOld_u20.csv", row.names = FALSE)
