################################################################################
#' @description Loads all libraries and inputs required for Prediction.
#' @return Inputs loaded below
################################################################################
# Clear environment
rm(list = ls())
#' Libraries
library(abind)
library(rstan)
library(readxl)
library(tidyr)
library(dplyr)
library(tibble) # rownames_to_column in fn_pci2
library(MASS) # mvrnorm in fn_pr2
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Hyperparameters
dat_filename <- list.files("./data/model-objects/")
dat_filename <- dat_filename[grepl("modelhyperparameters", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat_hp <- readRDS(paste0("./data/model-objects/",dat_filename))

# Prediction data
dat_pred <- read.csv("./gen/data-management/output/dat_pred_u20.csv")

# Classification keys
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")

# For capping malaria fractions
cases_malaria_05to19   <-  read.csv("./gen/prediction/input/cases_malaria_05to19.csv")
frac_malaria_01to04 <-  read.csv("./gen/prediction/input/frac_malaria_01to04.csv")

# # Classification keys
# key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageSexSuffix, ".csv", sep=""))
# key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
# 
# # Envelope
# env <- read.csv(paste("./gen/data-management/output/env_", ageSexSuffix, ".csv", sep=""))
# 
# # Model objects
# mod_fit_HMM <- readRDS(paste("./gen/prediction/input/mod_fit_", ageSexSuffix, "_HMM.rds", sep=""))
# mod_covNames_HMM <- mod_fit_HMM$param$VXF
# #mod_fit_LMM <- readRDS(paste("./gen/prediction/input/mod_fit_", ageSexSuffix, "_LMM.rds", sep=""))
# #mod_covNames_LMM <- mod_fit_LMM$param$VXF
################################################################################

