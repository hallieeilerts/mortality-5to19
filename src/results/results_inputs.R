################################################################################
#' @description Loads all libraries and inputs required for Results
#' @return Inputs loaded below
################################################################################
#' Libraries
library(data.table)
#' Inputs
source("./src/prepare-session/set-inputs.R")

# Functions
source("./src/results/fn_adjustCSMFZeroDeaths.R")
source("./src/results/fn_roundCSMFsqz.R")
source("./src/results/fn_publishEstimates.R")

# Classification keys
key_codlist       <- read.csv(paste("./gen/data-management/output/key_codlist_", ageSexSuffix, ".csv", sep=""))
key_region_u20    <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
key_agesexgrp     <- read.csv("./gen/data-management/output/key_agesexgrp_u20.csv")

# CSMFs that have been processed in squeezing pipeline (all countries, even those not subject to squeezing)
csmfPoint     <- read.csv(paste("./gen/squeezing/output/csmfSqz_", ageSexSuffix, ".csv", sep = ""))
csmfPoint_REG <- read.csv(paste("./gen/squeezing/output/csmfSqz_", ageSexSuffix, "_REG.csv", sep = ""))

# Point estimates, lower, and upper bounds for fractions/deaths/rates that have been processed in uncertainty pipeline
pointInt     <- read.csv(paste("./gen/uncertainty/output/pointInt_", ageSexSuffix, ".csv", sep = ""))
pointInt_REG <- read.csv(paste("./gen/uncertainty/output/pointInt_", ageSexSuffix, "REG.csv", sep = ""))
################################################################################