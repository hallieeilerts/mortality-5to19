################################################################################
#' @description Loads all libraries and inputs required for Estimation.
#' @return Inputs loaded below
################################################################################
# Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(rstan)
library(rstudioapi) # need only for estimation
library(callr) # need only for estimation
library(ggforce)  # need only for estimation?
library(bayesplot) # need only for estimation?
library(MCMCvis) # prob dont need
library(rlang) # not sure
library(here) # for st.patho
library(readxl)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Covariate names
dat_filename <- list.files("./data/keys/")
dat_filename <- dat_filename[grepl("CovariateDatabase2023_ModelCovariateList", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat_covar <- read_excel(paste0("./data/keys/",dat_filename), sheet = "model-covar-long")

# COD names
dat_filename <- list.files("./data/keys/")
dat_filename <- dat_filename[grepl("CODlist_ModeledReported", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat_cod <- read_excel(paste0("./data/keys/",dat_filename))

# Hyperparameters
dat_filename <- list.files("./data/model-objects/")
dat_filename <- dat_filename[grepl("modelhyperparameters", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat_hp <- readRDS(paste0("./data/model-objects/",dat_filename))

################################################################################




