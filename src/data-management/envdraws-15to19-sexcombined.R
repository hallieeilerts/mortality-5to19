################################################################################
#' @description Create 15-19 sex combined draws
#' @return Data frame with c("iso3", "Year", "Sex")
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
envDraws_15to19yF  <- readRDS(paste("./gen/data-management/output/envDraws_15to19yF.rds", sep=""))
envDraws_15to19yM  <- readRDS(paste("./gen/data-management/output/envDraws_15to19yM.rds", sep=""))
################################################################################

deaths2 <- map2(envDraws_15to19yF$deaths2, envDraws_15to19yM$deaths2, ~
                full_join(.x, .y, by = c("iso3", "year"), suffix = c("_F", "_M"))) 
rates2 <- map2(envDraws_15to19yF$rates2, envDraws_15to19yM$rates2, ~
                  full_join(.x, .y, by = c("iso3", "year"), suffix = c("_F", "_M"))) 
both <- map2(deaths2, rates2, ~
               full_join(.x, .y, by = c("iso3", "year"))) 

both <- lapply(both, function(x){ x$Px_F <- x$Deaths2_F/x$Rate2_F
                                  x$Px_M <- x$Deaths2_M/x$Rate2_M
                                  x$Px <- x$Px_F/x$Px_M
                                  x$Deaths2 <- x$Deaths2_F/x$Deaths2_M
                                  x$Rate2 <- x$Deaths2/x$Px
                                  x <- x[,c("iso3", "year", "Deaths2", "Rate2")];
                                  return(x)})


l_deaths2 <- lapply(both, function(x) x[,c("iso3", "year", "Deaths2")])
l_rates2 <- lapply(both, function(x) x[,c("iso3", "year", "Rate2")])

envDraws_15to19y <- list(deaths2 = l_deaths2,
                         rates2 = l_rates2)

# Save output(s) ----------------------------------------------------------

saveRDS(envDraws_15to19y, file = paste0("./gen/data-management/output/envDraws_15to19y.rds"))

