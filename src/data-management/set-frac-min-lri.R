################################################################################
#' @description Set minimum fractions for lri
#' @return scalar with frac_lri
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
################################################################################

# Minimum fraction of LRI
if(ageSexSuffix == "05to09y"){frac_lri <- 0.0269}
if(ageSexSuffix == "10to14y"){frac_lri <- 0.0197}
if(ageSexSuffix %in% c("15to19yF","15to19yM")){frac_lri <- NULL}

# Save output(s) ----------------------------------------------------------

saveRDS(frac_lri, file = paste("./gen/squeezing/input/frac_lri_", ageSexSuffix, ".rds", sep=""))