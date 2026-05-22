################################################################################
#' @description Load draws and save for years of interest
#' @return Draws for 2000-2024
################################################################################
#' Initialize environment
rm(list = ls())
#' Libraries
require(tidyr)
library(dplyr)
library(readxl)
#' Functions
source("./src/prepare-session/set-inputs.R")
#' Inputs
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("countrymodelclass", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1)
key_ctryclass_u20  <- read_excel(paste0("./data/keys/", dat_filename, sep = ""), sheet = "CountryModelClass")
dat_filename <- list.files("./data/keys")
dat_filename <- dat_filename[grepl("agesexgroups", dat_filename, ignore.case = TRUE)]
key_agesexgrps <- read.csv(paste0("./data/keys/", dat_filename, sep = ""))
################################################################################

v_folders <- c("5-9", "10-14", "15-19 girls", "15-19 boys")
v_AgeSexLabel <- c("Years5to9", "Years10to14","Years15to19f", "Years15to19m")

for(i in 1:length(v_folders)){
  
  myAgeSexSuffix <- subset(key_agesexgrps, AgeSexLabel == v_AgeSexLabel[i])$AgeSexSuffix
  print(myAgeSexSuffix)
  
  # crisis-free
  filepath <- paste0("./data/igme-draws/", v_folders[i], "/crisis-free/draws/")
  filenames <- list.files(filepath)
  file_cfdeaths <- filenames[grepl("death", filenames)]
  file_cfrates <- filenames[!grepl("death", filenames)]
  obj_name <- load(paste0(filepath, file_cfdeaths))
  dat_cfdeaths <- get(obj_name)
  obj_name <- load(paste0(filepath, file_cfrates))
  dat_cfrates <- get(obj_name)
  # limit to 2000-2024
  dat_cfdeaths <- dat_cfdeaths[, dim(dat_cfdeaths)[2] - (length(Years)-1):0,]
  dat_cfrates <- dat_cfrates[, dim(dat_cfrates)[2] - (length(Years)-1):0,]
  
  # crisis-included
  filepath <- paste0("./data/igme-draws/", v_folders[i], "/crisis-included/draws/")
  filenames <- list.files(filepath)
  file_cideaths <- filenames[grepl("death", filenames)]
  file_cirates <- filenames[!grepl("death", filenames)]
  obj_name <- load(paste0(filepath, file_cideaths))
  dat_cideaths <- get(obj_name)
  obj_name <- load(paste0(filepath, file_cirates))
  dat_cirates <- get(obj_name)
  dat_cideaths <- dat_cideaths[, dim(dat_cideaths)[2] - (length(Years)-1):0,]
  dat_cirates <- dat_cirates[, dim(dat_cirates)[2] - (length(Years)-1):0,]
  
  # Select countries --------------------------
  
  # Select countries of interest
  deaths1 <- dat_cfdeaths[which(dimnames(dat_cfdeaths)[[1]] %in% key_ctryclass_u20$ISO3), , ]
  rates1 <- dat_cfrates[which(dimnames(dat_cfrates)[[1]] %in% key_ctryclass_u20$ISO3), , ]
  deaths2  <- dat_cideaths[which(dimnames(dat_cideaths)[[1]] %in% key_ctryclass_u20$ISO3), , ]
  rates2  <- dat_cirates[which(dimnames(dat_cirates)[[1]] %in% key_ctryclass_u20$ISO3), , ]
  rm(dat_cfdeaths, dat_cfrates, dat_cideaths, dat_cirates)
  
  if(myAgeSexSuffix %in% c("05to09y", "10to14y")){
    
    # Exclude draws with inconsistencies
    # For 15-19f and 15-19m, this is done in the adjust-15to19-envelopes script, because want to exclude same draws for both
    
    # Where crisis-free envelopes are larger than crisis-included 
    dif <- deaths2 - deaths1
    idExclude <- c()
    for (i in 1:dim(dif)[3]) {
      if (any(dif[,,i] < 0, na.rm = T)) idExclude <- c(idExclude, i)
    }
    if(length(idExclude) > 0) {
      deaths1 <- deaths1[, , -idExclude]
      deaths2 <- deaths2[, , -idExclude]
      rates1  <- rates1[, , -idExclude]
      rates2  <- rates2[, , -idExclude]
    }
  }

  # Combine all
  envDraws <- list(deaths1 = deaths1, deaths2 = deaths2, rates1 = rates1, rates2 = rates2)
  
  # Save output(s) ----------------------------------------------------------
  
  saveRDS(envDraws, file = paste("./gen/data-management/temp/envDraws_", myAgeSexSuffix, ".rds",sep=""))

}
