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
library(MASS)
#' Inputs
source("./src/prepare-session/set-inputs.R")

# Hyperparameters
dat_filename <- list.files("./data/keys/")
dat_filename <- dat_filename[grepl("modelhyperparameters", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat_hp <- read.csv(paste0("./data/keys/",dat_filename))

# Prediction data
dat_pred <- read.csv("./gen/data-management/output/dat_pred_u20.csv")

# Classification keys
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")

# For capping malaria fractions
cases_malaria_05to19   <-  read.csv("./gen/prediction/input/cases_malaria_05to19.csv")
frac_malaria_01to04 <-  read.csv("./gen/prediction/input/frac_malaria_01to04.csv")
################################################################################

