################################################################################
#' @description Load draws and reformat
#' @return Reformatted draws
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

v_AgeSexLabel <- c("Years5to9", "Years10to14","Years15to19f", "Years15to19m")

for(i in 1:length(v_AgeSexLabel)){
  
  myAgeSexSuffix <- subset(key_agesexgrps, AgeSexLabel == v_AgeSexLabel[i])$AgeSexSuffix
  print(myAgeSexSuffix)
  
  if(i %in% 1:2){
    envDraws  <- readRDS(paste0("./gen/data-management/temp/envDraws_", myAgeSexSuffix, ".rds"))
  }
  if(i %in% 3:4){
    envDraws  <- readRDS(paste0("./gen/data-management/temp/envDrawsAdj_", myAgeSexSuffix, ".rds"))
  }
  
  deaths1 <- envDraws$deaths1
  deaths2 <- envDraws$deaths2
  rates1 <- envDraws$rates1
  rates2 <- envDraws$rates2
  
  # Check for NAs -----------------------------------------------------------
  
  if (any(anyNA(deaths1), anyNA(deaths2), anyNA(rates1), anyNA(rates2))) {
    warning("One or more arrays contain NA values.")
  }
  
  # Crisis-free deaths
  # Transform array into list of data frames
  l_deaths1 <- lapply(1:dim(deaths1)[3], function(x){ as.data.frame(deaths1[, , x]) })
  # Add ISO3 column to end of data frame
  l_deaths1 <- lapply(l_deaths1, function(x){ x$iso3 <- rownames(x) ; return(x)})
  l_deaths1 <- lapply(l_deaths1, function(x){ rownames(x) <- NULL ; return(x)})
  # Reshape to long
  l_deaths1 <- lapply(l_deaths1, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_deaths1 <- lapply(l_deaths1, function(x){ names(x)[names(x) == "values"] <- "Deaths1" ; return(x)})
  l_deaths1 <- lapply(l_deaths1, function(x){ names(x)[names(x) == "ind"]    <- "year" ; return(x)})
  
  # Crisis-free rates
  # Transform array into list of data frames
  l_rates1 <- lapply(1:dim(rates1)[3], function(x){ as.data.frame(rates1[, , x]) })
  # Add ISO3 column to end of data frame
  l_rates1 <- lapply(l_rates1, function(x){ x$iso3 <- rownames(x) ; return(x)})
  l_rates1 <- lapply(l_rates1, function(x){ rownames(x) <- NULL ; return(x)})
  # Reshape to long
  l_rates1 <- lapply(l_rates1, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_rates1 <- lapply(l_rates1, function(x){ names(x)[names(x) == "values"] <- "Rate2" ; return(x)})
  l_rates1 <- lapply(l_rates1, function(x){ names(x)[names(x) == "ind"]    <- "year" ; return(x)})
  
  # Crisis-included deaths
  # Transform array into list of data frames
  l_deaths2 <- lapply(1:dim(deaths2)[3], function(x){ as.data.frame(deaths2[, , x]) })
  # Add ISO3 column to end of data frame
  l_deaths2 <- lapply(l_deaths2, function(x){ x$iso3 <- rownames(x) ; return(x)})
  l_deaths2 <- lapply(l_deaths2, function(x){ rownames(x) <- NULL ; return(x)})
  # Reshape to long
  l_deaths2 <- lapply(l_deaths2, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_deaths2 <- lapply(l_deaths2, function(x){ names(x)[names(x) == "values"] <- "Deaths2" ; return(x)})
  l_deaths2 <- lapply(l_deaths2, function(x){ names(x)[names(x) == "ind"]    <- "year" ; return(x)})
  
  # Crisis-included rates
  # Transform array into list of data frames
  l_rates2 <- lapply(1:dim(rates2)[3], function(x){ as.data.frame(rates2[, , x]) })
  # Add ISO3 column to end of data frame
  l_rates2 <- lapply(l_rates2, function(x){ x$iso3 <- rownames(x) ; return(x)})
  l_rates2 <- lapply(l_rates2, function(x){ rownames(x) <- NULL ; return(x)})
  # Reshape to long
  l_rates2 <- lapply(l_rates2, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_rates2 <- lapply(l_rates2, function(x){ names(x)[names(x) == "values"] <- "Rate2" ; return(x)})
  l_rates2 <- lapply(l_rates2, function(x){ names(x)[names(x) == "ind"]    <- "year" ; return(x)})
  
  # Combine all
  envDraws <- list(deaths1 = l_deaths1, deaths2 = l_deaths2, rates1 = l_rates1, rates2 = l_rates2)
  
  # Check that all expected countries are included --------------------------
  
  if(sum(!(unique(key_ctryclass_u20$ISO3) %in% envDraws$deaths1[[1]]$iso3)) > 0){
    warning("Required countries missing from formatted envelopes.")
  }
  
  # Save output(s) ----------------------------------------------------------
  
  saveRDS(envDraws, paste0("./gen/data-management/output/envDraws_", myAgeSexSuffix, ".rds"))
  
}

# Clear temp folder -------------------------------------------------------

files <- list.files("./gen/data-management/temp/", full.names = TRUE)
files_to_remove <- files[!grepl("\\.gitkeep$", files)]
file.remove(files_to_remove)

