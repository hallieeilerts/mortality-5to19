################################################################################
#' @description Pull inputs from Data Warehouse
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
#' Functions 
source("./src/util.R")
################################################################################

# # Classification keys
# fn_initEnvironmentData("classification-keys")
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/classification-keys/age-sex-groups", sep = ""))
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/classification-keys/age-sex-groups",dat_filename[i], sep = ""),
#             to   = paste0("./data/classification-keys",dat_filename[i]))
# }
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/classification-keys/model-class", sep = ""))
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/classification-keys/model-class",dat_filename[i], sep = ""),
#             to   = paste0("./data/classification-keys",dat_filename[i]))
# }
# 
# # Crisis
# fn_initEnvironmentData("crisis")
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/data/single-causes/crisis", sep = ""))
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2021/data/single-causes/crisis",dat_filename[i], sep = ""),
#             to   = paste0("./data/crisis",dat_filename[i]))
# }
# 
# # HIV
# fn_initEnvironmentData("hiv")
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/data/single-causes/hiv", sep = ""))
# dat_filename <- dat_filename[grepl("wppfractions", dat_filename, ignore.case = TRUE)]
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2021/data/single-causes/hiv",dat_filename[i], sep = ""),
#             to   = paste0("./data/hiv",dat_filename[i]))
# }
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/single-causes/hiv", sep = ""))
# dat_filename <- dat_filename[grepl("hiv2024", dat_filename, ignore.case = TRUE)]
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/single-causes/hiv",dat_filename[i], sep = ""),
#             to   = paste0("./data/hiv",dat_filename[i]))
# }
# 
# # Malaria
# fn_initEnvironmentData("malaria")
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/single-causes/malaria", sep = ""))
# dat_filename <- dat_filename[grepl("xlsx", dat_filename, ignore.case = TRUE)]
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/single-causes/malaria",dat_filename[i], sep = ""),
#             to   = paste0("./data/malaria",dat_filename[i]))
# }
# 
# # Malaria cases
# fn_initEnvironmentData("malaria-cases")
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/data/single-causes/malaria-cases", sep = ""))
# dat_filename <- dat_filename[grepl("xlsx", dat_filename, ignore.case = TRUE)]
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2021/data/single-causes/malaria-cases",dat_filename[i], sep = ""),
#             to   = paste0("./data/malaria-cases",dat_filename[i]))
# }
# 
# # Measles
# fn_initEnvironmentData("measles")
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/data/single-causes/measles", sep = ""))
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2021/data/single-causes/measles",dat_filename[i], sep = ""),
#             to   = paste0("./data/measles",dat_filename[i]))
# }
# 
# # TB
# fn_initEnvironmentData("tb")
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/data/single-causes/tb", sep = ""))
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2021/data/single-causes/tb",dat_filename[i], sep = ""),
#             to   = paste0("./data/tb",dat_filename[i]))
# }
# 
# # IGME envelopes
# fn_initEnvironmentData("igme/envelopes/national")
# dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/igme", sep = ""))
# for(i in 1:length(dat_filename)){
#   file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/igme",dat_filename[i], sep = ""),
#             to   = paste0("./data/igme/envelopes/national",dat_filename[i]))
# }

# China DSP