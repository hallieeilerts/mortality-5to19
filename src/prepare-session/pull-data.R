################################################################################
#' @description Pull data from data warehouse
#' @return Data in /data subdirectories
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(stringr)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
source("./src/prepare-session/prepare-session_functions.R")
################################################################################

# Prediction database
fn_initEnvironmentData("prediction-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/prediction-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/prediction-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/prediction-database/",dat_filename[i]))
}

# Study database
fn_initEnvironmentData("model-objects")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/model-objects", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/model-objects/",dat_filename[i], sep = ""),
            to   = paste0("./data/model-objects/",dat_filename[i]))
}

# Study database 
# Including just for now while testing covariates in estimation/check-colinearity
# same goes for lmm-database
fn_initEnvironmentData("study-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/study-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/study-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/study-database/",dat_filename[i]))
}

# Classification keys
fn_initEnvironmentData("classification-keys")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/keys", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/keys/",dat_filename[i], sep = ""),
            to   = paste0("./data/classification-keys/",dat_filename[i]))
}

# Single causes database
fn_initEnvironmentData("single-causes")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/single-causes", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/single-causes/",dat_filename[i], sep = ""),
            to   = paste0("./data/single-causes/",dat_filename[i]))
}

