################################################################################
#' @description Loads all libraries and inputs required for Visualizations
#' @return Inputs loaded below
################################################################################
# Clear environment
rm(list = ls())
#' Libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
#library(plyr) # dlply
library(data.table) # melt, dcast
#' Inputs
source("./src/prepare-session/set-inputs.R")

# Classification keys
key_ctryclassOld <- read.csv("./gen/data-management/output/key_ctryclassOld_u20.csv")
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")

#' Estimates that have been processed in squeezing pipeline and formatted in results (intermediate results)
#' Note: Use these for creating visualizations when uncertainty pipeline isn't ready.
#' Some point estimates for fractions will be slightly different from the results produced in the uncertainty pipeline.
#' The function fn_adjustCSMFZeroDeaths performs some adjustments that happen in fn_adjustPointIntZeroDeaths(), but not all. 
#point <- read.csv(paste("./gen/results/temp/Test10-PointEstimates_National_", ageSexSuffix,"_20241109.csv", sep="")) # Used in Barcelona meeting
#point_REG <- read.csv(paste("./gen/results/temp/PointEstimates_Regional_", ageSexSuffix,"_20241006.csv", sep=""))

# IMPORTANT NOTE FOR BUILD-UP PLOTS
# the estimates labeled "new single causes" actually incorporate an additional change, and that is how the random effects were fit.
# because at this point, the estimates become more bumpy for certain causes (see 10-14y BGD OtherCMPN) and it's actually not due to the single cause data.
# See JHU team meeting notes from June 17 for discussions of this change to fitting RE. So i must have implemented this change sometime
# after the June estimates and things got bumpy, but I didn't really notice it.
# would be ideal to have a set of estimates that were done with the new covar set, old single causes and old hyperparam. I'm not quite sure what single cause database was in use at the time though.
# See Objective 1 meeting notes on June 26 for more details.

# full update with old covar set for 5-9
if(ageSexSuffix == "05to09y"){
  #point2 <- read.csv(paste("./gen/results/temp/Test2-PointEstimates_National_", ageSexSuffix,"_20250225.csv", sep=""))
  # full update testing new covar set for 5-9, 10-14, 15-19f
  #point3 <- read.csv(paste("./gen/results/temp/Test3-PointEstimates_National_", ageSexSuffix,"_20250514.csv", sep=""))
  # full update testing new covar for 5-9 and 10-14 as of 20250612 (hopefully final covar for these ages)
  #point4 <- read.csv(paste("./gen/results/temp/Test4-PointEstimates_National_", ageSexSuffix,"_20250616.csv", sep=""))
  # full update testing new covar with country-level random effects
  #point5 <- read.csv(paste("./gen/results/temp/Test5-PointEstimates_National_", ageSexSuffix,"_20250623.csv", sep=""))
  # full update testing new hyperparameters!
  #point6 <- read.csv(paste("./gen/results/temp/NewHP-PointEstimates_National_", ageSexSuffix,"_20250818.csv", sep=""))
  # full update testing new hyperparameters including LMM
  #point5 <- read.csv(paste("./gen/results/temp/NewHP-PointEstimates_National_", ageSexSuffix,"_20250821.csv", sep=""))
  #point5 <- read.csv(paste("./gen/results/temp/NewHPmed-PointEstimates_National_", ageSexSuffix,"_20250909.csv", sep=""))
  #point5 <- read.csv(paste("./gen/results/temp/NewHPcrisis-PointEstimates_National_", ageSexSuffix,"_20250911.csv", sep=""))
  # new studies, new covar values
  new_covarval <- read.csv(paste("./gen/results/temp/Test2-PointEstimates_National_", ageSexSuffix,"_20250225.csv", sep=""))
  # new covar set and country-level random effects
  new_covarset <- read.csv(paste("./gen/results/temp/Test5-PointEstimates_National_", ageSexSuffix,"_20250623.csv", sep=""))
  # first in stan. should be using the same single causes as new_covarset. new_covarval had some different single causes.
  new_stan <- read.csv(paste("./gen/results/temp/FirstStan-PointEstimates_National_", ageSexSuffix,"_20250919.csv", sep=""))
  # new single causes and stan
  new_sc <- read.csv(paste("./gen/results/temp/NewSC-PointEstimates_National_", ageSexSuffix,"_20250918.csv", sep=""))
  # new hyperparameters
  new_hp <- read.csv(paste("./gen/results/temp/NewHP-PointEstimates_National_", ageSexSuffix,"_20250915.csv", sep=""))
  # changed pred functions to use smooth covariates.
  new_sm <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922.csv", sep=""))
  # extrapolated vr to 2024
  new_vr <- read.csv(paste("./gen/results/temp/PointEstimates_National_", ageSexSuffix,"_20250926.csv", sep=""))
  # regional results
  pointReg <- read.csv(paste("./gen/results/output/PointEstimates_Regional_", ageSexSuffix,"_20251022.csv", sep=""))
  
}
if(ageSexSuffix == "10to14y"){
  # new studies, new covar values
  #point2 <- read.csv(paste("./gen/results/temp/Test2-PointEstimates_National_", ageSexSuffix,"_20250227.csv", sep=""))
  # # full update testing new covar set for 5-9, 10-14, 15-19f
  # point3 <- read.csv(paste("./gen/results/temp/Test3-PointEstimates_National_", ageSexSuffix,"_20250514.csv", sep=""))
  # full update testing new covar for 5-9 and 10-14 as of 20250612 (hopefully final covar for these ages) (new studies and covariate set)
  # new studies, new covar values
  new_covarval <- read.csv(paste("./gen/results/temp/Test2-PointEstimates_National_", ageSexSuffix,"_20250227.csv", sep=""))
  # new covar set and country-level random effects
  new_covarset <- read.csv(paste("./gen/results/temp/Test4-PointEstimates_National_", ageSexSuffix,"_20250616.csv", sep=""))
  # first in stan. should be using the same single causes as new_covarset. new_covarval had some different single causes.
  new_stan <- read.csv(paste("./gen/results/temp/FirstStan-PointEstimates_National_", ageSexSuffix,"_20250919.csv", sep=""))
  # new single causes and stan
  new_sc <- read.csv(paste("./gen/results/temp/NewSC-PointEstimates_National_", ageSexSuffix,"_20250918.csv", sep=""))
  # new hyperparameters (new studies, covariate set, all single cause data and crisis squeezing)
  new_hp <- read.csv(paste("./gen/results/temp/NewHP-PointEstimates_National_", ageSexSuffix,"_20250917.csv", sep=""))
  # alternate lambdas  (lower or higher than new value)
  alt_lam_higher <- read.csv(paste("./gen/results/temp/UpTo2024-lam30-PointEstimates_National_", ageSexSuffix,"_20250917.csv", sep="")) 
  # changed pred functions to use smooth covariates.
  new_sm <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922.csv", sep=""))
  # extrapolated vr to 2024
  new_vr <- read.csv(paste("./gen/results/temp/PointEstimates_National_", ageSexSuffix,"_20250926.csv", sep=""))
  # regional results
  pointReg <- read.csv(paste("./gen/results/output/PointEstimates_Regional_", ageSexSuffix,"_20251022.csv", sep=""))
}
if(ageSexSuffix %in% c("15to19yF")){
  # new studies, new covar values
  new_covarval <- read.csv(paste("./gen/results/temp/Test2-PointEstimates_National_", ageSexSuffix,"_20250514.csv", sep=""))
  # new covar set
  new_covarset <- read.csv(paste("./gen/results/temp/Test3-PointEstimates_National_", ageSexSuffix,"_20250514.csv", sep=""))
  # first in stan. should be using the same single causes as new_covarset. new_covarval had some different single causes.
  new_stan <- read.csv(paste("./gen/results/temp/FirstStan-PointEstimates_National_", ageSexSuffix,"_20250919.csv", sep=""))
  # new single causes and stan
  #new_sc <- read.csv(paste("./gen/results/temp/NewSC-PointEstimates_National_", ageSexSuffix,"_20250918.csv", sep=""))
  # new single causes and stan - tb fixed
  new_sc <- read.csv(paste("./gen/results/temp/NewSC-PointEstimates_National_", ageSexSuffix,"_20250919.csv", sep=""))
  # new hyperparameters
  #new_hp <- read.csv(paste("./gen/results/temp/UpTo2024-PointEstimates_National_", ageSexSuffix,"_20250915.csv", sep=""))
  # new hyperparameters - tb fixed
  new_hp <- read.csv(paste("./gen/results/temp/NewHP-PointEstimates_National_", ageSexSuffix,"_20250919.csv", sep=""))
  # changed pred functions to use smooth covariates.
  new_sm <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922.csv", sep=""))
  # extrapolated vr to 2024
  new_vr <- read.csv(paste("./gen/results/temp/PointEstimates_National_", ageSexSuffix,"_20250926.csv", sep=""))
  # regional results
  pointReg <- read.csv(paste("./gen/results/output/PointEstimates_Regional_", ageSexSuffix,"_20251022.csv", sep=""))
}
if(ageSexSuffix %in% c("15to19yM")){
  # new studies, new covar values
  new_covarval <- read.csv(paste("./gen/results/temp/Test2-PointEstimates_National_", ageSexSuffix,"_20250514.csv", sep=""))
  # new covar set
  new_covarset <- read.csv(paste("./gen/results/temp/Test3-PointEstimates_National_", ageSexSuffix,"_20250514.csv", sep=""))
  # first in stan. should be using the same single causes as new_covarset. new_covarval had some different single causes.
  new_stan <- read.csv(paste("./gen/results/temp/FirstStan-PointEstimates_National_", ageSexSuffix,"_20250919.csv", sep=""))
  # new single causes and stan
  #new_sc <- read.csv(paste("./gen/results/temp/UpTo2024-lam400-PointEstimates_National_", ageSexSuffix,"_20250917.csv", sep=""))
  # new single causes and stan - tb fixed
  new_sc <- read.csv(paste("./gen/results/temp/NewSC-PointEstimates_National_", ageSexSuffix,"_20250919.csv", sep=""))
  # new hyperparameters
  # new_hp <- read.csv(paste("./gen/results/temp/UpTo2024-PointEstimates_National_", ageSexSuffix,"_20250917.csv", sep=""))
  # new hyperparameters - tb fixed
  new_hp <- read.csv(paste("./gen/results/temp/NewHP-PointEstimates_National_", ageSexSuffix,"_20250919.csv", sep=""))
  # # alternate lambdas (lower or higher than new value)
  # alt_lam_lower <- read.csv(paste("./gen/results/temp/UpTo2024-lam10-PointEstimates_National_", ageSexSuffix,"_20250917.csv", sep=""))
  # alt_lam_higher <- read.csv(paste("./gen/results/temp/UpTo2024-lam20-PointEstimates_National_", ageSexSuffix,"_20250917.csv", sep="")) 
  # changed pred functions to use smooth covariates 
  new_sm <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922-lam400.csv", sep=""))
  # extrapolated vr to 2024
  new_vr <- read.csv(paste("./gen/results/temp/PointEstimates_National_", ageSexSuffix,"_20250926.csv", sep=""))
  # regional results
  pointReg <- read.csv(paste("./gen/results/output/PointEstimates_Regional_", ageSexSuffix,"_20251022.csv", sep=""))
}

# TURNING OFF FOR BARCELONA MEETING PREP
#' #' Estimates that have been processed in uncertainty pipeline and formatted in results (final results)
#' point <- read.csv(paste("./gen/results/output/PointEstimates_National_", ageSexSuffix,"_20231002.csv", sep=""))
#' point_REG <- read.csv(paste("./gen/results/output/PointEstimates_Regional_", ageSexSuffix,"_20231002.csv", sep=""))
#' pointInt <- read.csv(paste("./gen/results/output/Uncertainty_National_", ageSexSuffix, "_20231002.csv", sep = ""))
#' pointInt_REG <- read.csv(paste("./gen/results/output/Uncertainty_Regional_", ageSexSuffix, "_20231002.csv", sep = ""))

#' #' Pancho's estimates from 2000-2019 estimation round
#' if(ageSexSuffix == "05to09y"){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates5to9-National.csv")
#'                               point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates5to9-Regional.csv")}
#' if(ageSexSuffix == "10to14y"){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates10to14-National.csv")
#'                               point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates10to14-Regional.csv")}
#' if(ageSexSuffix %in% c("15to19yF", "15to19yM")){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates15to19-National.csv")
#'                                                 point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates15to19-Regional.csv")}

#' Pancho's estimates from 2000-2021 estimation round
if(ageSexSuffix == "05to09y"){matchdis <- read.csv("./data/previous-results/2000-2021/_old/PointEstimates5to9-National.csv")
                         point_PanchoResults <- read.csv("./data/previous-results/2000-2021/CA-CODE-2024-5to9-National.csv")
                         #point_PanchoResults_REG <- read.csv("./data/previous-results/2000-2021/_old/PointEstimates5to9-Regional.csv")
                         #pointInt_PanchoResults <- read.csv("./data/previous-results/2000-2021/_old/Uncertainty5to9-National.csv")
                         }
if(ageSexSuffix == "10to14y"){
                         #point_PanchoResults <- read.csv("./data/previous-results/2000-2021/_old/PointEstimates10to14-National.csv")
                         point_PanchoResults <- read.csv("./data/previous-results/CA-CODE-2024-10to14-National.csv")
                         #point_PanchoResults_REG <- read.csv("./data/previous-results/2000-2021/_old/PointEstimates10to14-Regional.csv")
                         #pointInt_PanchoResults <- read.csv("./data/previous-results/2000-2021/_old/Uncertainty10to14-National.csv")
                         }
if(ageSexSuffix %in% c("15to19yF", "15to19yM")){
                         #point_PanchoResults <- read.csv("./data/previous-results/2000-2021/_old/PointEstimates15to19-National.csv")
                         point_PanchoResults <- read.csv("./data/previous-results/CA-CODE-2024-15to19-National.csv")
                         #point_PanchoResults_REG <- read.csv("./data/previous-results/2000-2021/_old/PointEstimates15to19-Regional.csv")
                         #pointInt_PanchoResults <- read.csv("./data/previous-results/2000-2021/_old/Uncertainty15to19-National.csv")
                         }
################################################################################