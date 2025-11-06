################################################################################
#' @description Pull data from data warehouse
#' @return Data in /data subdirectories
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(stringr)
library(fs)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/prepare-session_functions.R")
################################################################################

# Classification keys
fn_initEnvironmentData("keys")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2024/keys", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2024/keys/",dat_filename[i], sep = ""),
            to   = paste0("./data/keys/",dat_filename[i]))
}

# Model inputs
fn_initEnvironmentData("model-objects")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2024/model-pipeline/model-objects", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2024/model-pipeline/model-objects/",dat_filename[i], sep = ""),
            to   = paste0("./data/model-objects/",dat_filename[i]))
}

# Good VR data
fn_initEnvironmentData("vr")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2024/model-pipeline/vr", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2024/model-pipeline/vr/",dat_filename[i], sep = ""),
            to   = paste0("./data/vr/",dat_filename[i]))
}

# China DSP
fn_initEnvironmentData("china")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2024/data/china", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2024/data/china/",dat_filename[i], sep = ""),
            to   = paste0("./data/china/",dat_filename[i]))
}

# Prediction database
fn_initEnvironmentData("prediction-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2024/model-pipeline/prediction-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2024/model-pipeline/prediction-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/prediction-database/",dat_filename[i]))
}

# Single cause database
fn_initEnvironmentData("single-causes")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2024/model-pipeline/single-causes", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2024/model-pipeline/single-causes/",dat_filename[i], sep = ""),
            to   = paste0("./data/single-causes/",dat_filename[i]))
}

# IGME draws
fn_initEnvironmentData("igme-draws")
for (folder in c("5-9", "10-14", "15-19 girls", "15-19 boys")) {
  dir_copy(
    path(file.path(pathDataWarehouse, "2000-2024/data/igme", folder)),
    path(file.path("./data/igme-draws", folder)),
    overwrite = TRUE
  )
}

# Previous results
fn_initEnvironmentData("previous-results")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/results", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2021/results/",dat_filename[i], sep = ""),
            to   = paste0("./data/previous-results/",dat_filename[i]))
}




