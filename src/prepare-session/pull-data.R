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
source("./src/prepare-session/prepare-session_functions.R")
################################################################################

# Classification keys
fn_initEnvironmentData("keys")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/keys", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/keys/",dat_filename[i], sep = ""),
            to   = paste0("./data/keys/",dat_filename[i]))
}

# HMM (study) database 
fn_initEnvironmentData("study-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/study-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/study-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/study-database/",dat_filename[i]))
}

# LMM database
fn_initEnvironmentData("lmm-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/lmm-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/lmm-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/lmm-database/",dat_filename[i]))
}

# Good VR data
fn_initEnvironmentData("vr")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/vr", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/vr/",dat_filename[i], sep = ""),
            to   = paste0("./data/vr/",dat_filename[i]))
}

# China DSP
fn_initEnvironmentData("china")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/china", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/china/",dat_filename[i], sep = ""),
            to   = paste0("./data/china/",dat_filename[i]))
}

# Prediction database
fn_initEnvironmentData("prediction-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/prediction-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/prediction-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/prediction-database/",dat_filename[i]))
}

# Single cause database
fn_initEnvironmentData("single-causes")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/single-causes", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/single-causes/",dat_filename[i], sep = ""),
            to   = paste0("./data/single-causes/",dat_filename[i]))
}

# IGME draws

# Previous results
