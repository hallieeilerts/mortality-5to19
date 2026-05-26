################################################################################
#' @description Loads all libraries and inputs required for Uncertainty
#' @return Inputs loaded below
################################################################################

#' Libraries
library(msm)
library(data.table)
#' Inputs
source("./src/prepare-session/set-inputs.R")

# Functions
source("./src/uncertainty/fn_nestedLapply.R")
source("./src/uncertainty/fn_rearrangeDraws.R")
source("./src/uncertainty/fn_createSampleVectors.R")
source("./src/uncertainty/fn_randDrawEnv.R")
source("./src/uncertainty/fn_randAssignVR.R")
source("./src/uncertainty/fn_formatDraws.R")
source("./src/uncertainty/fn_sampleLogNorm.R")
source("./src/uncertainty/fn_randAssignMeas.R")
source("./src/uncertainty/fn_randAssignTB.R")
source("./src/uncertainty/fn_randAssignHIV.R")
source("./src/uncertainty/fn_calcUI.R")
source("./src/uncertainty/fn_combineUIpoint.R")
source("./src/uncertainty/fn_roundPointInt.R")
source("./src/uncertainty/fn_checkUI.R")
source("./src/uncertainty/fn_adjustPointIntZeroDeaths.R")
source("./src/uncertainty/fn_manuallyAdjustBounds.R")

# CSMF point estimates from squeezing
csmfPoint <- read.csv(paste("./gen/squeezing/output/csmfSqz_", ageSexSuffix, ".csv", sep = ""))
csmfPoint_REG <- read.csv(paste("./gen/squeezing/output/csmfSqz_", ageSexSuffix, "_REG.csv", sep = ""))

# CSMF draws from prediction
csmfDraws <- readRDS(paste("./gen/uncertainty/input/csmfDraws_", ageSexSuffix, ".rds", sep = ""))

# Envelope draws
envDraws  <- readRDS(paste("./gen/data-management/output/envDraws_", ageSexSuffix, ".rds", sep=""))

# COD reclassification key
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageSexSuffix, ".csv", sep=""))
################################################################################