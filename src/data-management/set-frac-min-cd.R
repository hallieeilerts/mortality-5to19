################################################################################
#' @description Set minimum fractions for communicable diseases
#' @return scalar with frac_cd
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
################################################################################

# See file 'Code/20201023_ReadTBdata.R'

# Minimum fraction of Other CD
if(ageSexSuffix == "05to09y"){frac_cd <- 0.0455}
if(ageSexSuffix == "10to14y"){frac_cd <- 0.0332}
if(ageSexSuffix == "15to19yF"){frac_cd <- 0.0474}
if(ageSexSuffix == "15to19yM"){frac_cd <- 0.0279}

# !!! can this be coded up?

# Save output(s) ----------------------------------------------------------

saveRDS(frac_cd, file = paste("./gen/squeezing/input/frac_cd_", ageSexSuffix, ".rds", sep=""))
#save(frac_cd, file = paste("./gen/squeezing/input/frac_cd_", ageSexSuffix, ".RData", sep=""))
