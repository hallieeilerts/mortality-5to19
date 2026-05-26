################################################################################
#' @description Loads all libraries and inputs required for Estimation.
#' @return Inputs loaded below
################################################################################

#' Libraries
library(tidyr)
library(dplyr)
library(rstan)
library(rstudioapi)
library(here)
library(readxl)
#' Inputs
source("./src/prepare-session/set-inputs.R")

# Functions
source("./src/estimation/fn_createModInput.R")
source("./src/estimation/fn_setRefCat.R")

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
dat_filename <- list.files("./data/keys/")
dat_filename <- dat_filename[grepl("modelhyperparameters", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat_hp <- read.csv(paste0("./data/keys/",dat_filename))

# Model objects HMM
dat_filename <- list.files("./data/model-objects/")
dat_filename <- dat_filename[grepl("deaths", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl("HMM", dat_filename)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
load(paste0("./data/model-objects/", dat_filename, sep = ""))
dat_filename <- list.files("./data/model-objects/")
dat_filename <- dat_filename[grepl("studies", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl("HMM", dat_filename)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
load(paste0("./data/model-objects/",dat_filename, sep = ""))
mod_dat_HMM <- list(deaths, studies)
names(mod_dat_HMM) <- c("deaths", "studies")

# Model objects LMM
dat_filename <- list.files("./data/model-objects/")
dat_filename <- dat_filename[grepl("deaths", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl("LMM", dat_filename)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
load(paste0("./data/model-objects/", dat_filename, sep = ""))
dat_filename <- list.files("./data/model-objects/")
dat_filename <- dat_filename[grepl("studies", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl("LMM", dat_filename)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
load(paste0("./data/model-objects/",dat_filename, sep = ""))
mod_dat_LMM <- list(deaths, studies)
names(mod_dat_LMM) <- c("deaths", "studies")
################################################################################




