################################################################################
#' @description Loads all libraries and inputs required for Uncertainty
#' @return Inputs loaded below
################################################################################
# Clear environment
rm(list = ls())
#' Libraries
library(msm)   # rtnorm()
#' Inputs
source("./src/prepare-session/set-inputs.R")

## Uncertainty inputs

# CSMF draws from prediction
csmfDraws <- readRDS(paste("./gen/uncertainty/input/csmfDraws_", ageSexSuffix, ".rds", sep = ""))

# Envelope draws
envDraws  <- readRDS(paste("./gen/data-management/output/envDraws_", ageSexSuffix, ".rds", sep=""))

# COD reclassification key
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageSexSuffix, ".csv", sep=""))

## Squeezing inputs

# Classification keys
key_region <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_codlist <- read.csv(paste("./gen/data-management/output/key_codlist_", ageSexSuffix, ".csv", sep=""))

# Envelopes
env <- read.csv(paste("./gen/data-management/output/env_", ageSexSuffix, ".csv", sep=""))
env_REG <- read.csv(paste("./gen/data-management/output/env_", ageSexSuffix, "_REG.csv", sep=""))

# VR data
csmf_GOODVR  <- read.csv(paste("./gen/data-management/output/csmf_vr_", ageSexSuffix, ".csv", sep=""))
csmf_CHN <- read.csv(paste("./gen/data-management/output/csmf_ChinaDSP_", ageSexSuffix, ".csv", sep=""))

# Single cause data
dat_tb     <- read.csv(paste("./gen/squeezing/input/dat_tb_", ageSexSuffix, ".csv", sep=""))
dat_hiv    <- read.csv(paste("./gen/squeezing/input/dat_hiv_", ageSexSuffix, ".csv", sep=""))
dat_crisis <- read.csv(paste("./gen/squeezing/input/dat_crisis_", ageSexSuffix, ".csv", sep=""))
if(ageSexSuffix == "05to09y"){dat_meas <- read.csv(paste("./gen/squeezing/input/dat_meas_05to09y.csv", sep=""))}
if(ageSexSuffix %in% c("10to14y","15to19yF", "15to19yM")){dat_meas <- NULL}

# Minimum fractions
frac_cd  <- readRDS(paste("./gen/squeezing/input/frac_cd_", ageSexSuffix, ".rds", sep=""))
frac_lri <- readRDS(paste("./gen/squeezing/input/frac_lri_", ageSexSuffix, ".rds", sep=""))
################################################################################