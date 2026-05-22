################################################################################
#' @description Drop draws with inconsistencies
#' @return Draws for 15-19f and 15-19m without inconsistencies
################################################################################
#' Initialize environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(purrr)
#' Functions
source("./src/prepare-session/set-inputs.R")
#' Inputs
env                <- read.csv("./gen/data-management/output/env_u20.csv")
envDraws_15to19yF  <- readRDS(paste("./gen/data-management/temp/envDraws_15to19yF.rds", sep=""))
envDraws_15to19yM  <- readRDS(paste("./gen/data-management/temp/envDraws_15to19yM.rds", sep=""))
################################################################################

# Extract draws -----------------------------------------------------------

deaths1Wom <- envDraws_15to19yF$deaths1
deaths2Wom <- envDraws_15to19yF$deaths2
rates1Wom <- envDraws_15to19yF$rates1
rates2Wom <- envDraws_15to19yF$rates2
deaths1Men <- envDraws_15to19yM$deaths1
deaths2Men <- envDraws_15to19yM$deaths2
rates1Men <- envDraws_15to19yM$rates1
rates2Men <- envDraws_15to19yM$rates2
rm(envDraws_15to19yF, envDraws_15to19yM)

# Exclude draws with inconsistencies --------------------------------------

# Exclude from both sexes so they are the arrays are the same size

dif <- deaths2Men - deaths1Men
idExcludeMen <- c()
for (i in 1:dim(dif)[3]) {
  if (any(dif[,,i] < 0, na.rm = T)) idExcludeMen <- c(idExcludeMen, i)
}
if (length(idExcludeMen) > 0) {
  
  deaths1Men <- deaths1Men[, , -idExcludeMen]
  deaths2Men <- deaths2Men[, , -idExcludeMen]
  rates1Men  <- rates1Men[, , -idExcludeMen]
  rates2Men  <- rates2Men[, , -idExcludeMen]
  
  deaths1Wom <- deaths1Wom[, , -idExcludeMen]
  deaths2Wom <- deaths2Wom[, , -idExcludeMen]
  rates1Wom  <- rates1Wom[, , -idExcludeMen]
  rates2Wom  <- rates2Wom[, , -idExcludeMen]
  
}

dif <- deaths2Wom - deaths1Wom
idExcludeWom <- c()
for (i in 1:dim(dif)[3]) {
  if (any(dif[,,i] < 0, na.rm = T)) idExcludeWom <- c(idExcludeWom, i)
}

if (length(idExcludeWom) > 0) {
  
  deaths1Men <- deaths1Men[, , -idExcludeWom]
  deaths2Men <- deaths2Men[, , -idExcludeWom]
  rates1Men  <- rates1Men[, , -idExcludeWom]
  rates2Men  <- rates2Men[, , -idExcludeWom]
  
  deaths1Wom <- deaths1Wom[, , -idExcludeWom]
  deaths2Wom <- deaths2Wom[, , -idExcludeWom]
  rates1Wom  <- rates1Wom[, , -idExcludeWom]
  rates2Wom  <- rates2Wom[, , -idExcludeWom]
}

# Drop draws with NA values for either sex ---------------

na_draws1 <- which(apply(is.na(deaths1Wom), 3, any))
na_draws2 <- which(apply(is.na(deaths2Wom), 3, any))
na_draws3 <- which(apply(is.na(rates1Wom), 3, any))
na_draws4 <- which(apply(is.na(rates2Wom), 3, any))
na_draws5 <- which(apply(is.na(deaths1Men), 3, any))
na_draws6 <- which(apply(is.na(deaths2Men), 3, any))
na_draws7 <- which(apply(is.na(rates1Men), 3, any))
na_draws8 <- which(apply(is.na(rates2Men), 3, any))

idExclude <- unique(c(na_draws1, na_draws2, na_draws3, na_draws4, na_draws5, na_draws6, na_draws7, na_draws8))

if (length(idExclude) > 0) {
  
  deaths1Men <- deaths1Men[, , -idExclude]
  deaths2Men <- deaths2Men[, , -idExclude]
  rates1Men  <- rates1Men[, , -idExclude]
  rates2Men  <- rates2Men[, , -idExclude]
  
  deaths1Wom <- deaths1Wom[, , -idExclude]
  deaths2Wom <- deaths2Wom[, , -idExclude]
  rates1Wom  <- rates1Wom[, , -idExclude]
  rates2Wom  <- rates2Wom[, , -idExclude]
}

envDraws_15to19yF <- list(deaths1 = deaths1Wom, deaths2 = deaths2Wom, rates1 = rates1Wom, rates2 = rates2Wom)
envDraws_15to19yM <- list(deaths1 = deaths1Men, deaths2 = deaths2Men, rates1 = rates1Men, rates2 = rates2Men)

# Save output(s) ----------------------------------------------------------

saveRDS(envDraws_15to19yF, file = paste("./gen/data-management/temp/envDrawsAdj_15to19yF.rds",sep=""))
saveRDS(envDraws_15to19yM, file = paste("./gen/data-management/temp/envDrawsAdj_15to19yM.rds",sep=""))
