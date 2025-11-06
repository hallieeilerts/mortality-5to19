################################################################################
#' @description Drop draws with inconsistencies
#' Fill in NA values for 15-19 sex-specific using ratio of male:female (currently not possible because dont have sex-combined draws)
#' @return 
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

# Exclude from both sexes so they are the same length if need to combine

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

# Use ratio of male:female 15-19 envelopes to fill in NAs in draws --------

# Note: Can't do this currently, because sex-combined draws not provided

# wom <- subset(env, AgeSexLabel == "Years15to19f")[, c("iso3", "year", "Deaths1", "Rate1", "Deaths2", "Rate2")]
# men <- subset(env, AgeSexLabel == "Years15to19m")[, c("iso3", "year", "Deaths1", "Rate1", "Deaths2", "Rate2")]
# 
# # Ratios for deaths (women/both)
# ratios <- merge(wom, men, by = c("iso3", "year"))
# ratios$Ratio1 <- ratios$Deaths1.x / (ratios$Deaths1.x + ratios$Deaths1.y)
# ratios$Ratio2 <- ratios$Deaths2.x / (ratios$Deaths2.x + ratios$Deaths2.y)
# 
# # Ratios for probabilities
# ratioQ <- ratios$Rate2.x / ratios$Rate2.y
# ratios$RQwom <- (ratios$Deaths2.x + ratios$Deaths2.y * ratioQ) /
#   (ratios$Deaths2.x + ratios$Deaths2.y)
# ratios$RQmen <- (ratios$Deaths2.x / ratioQ + ratios$Deaths2.y) /
#   (ratios$Deaths2.x + ratios$Deaths2.y)
# 
# # Split equally when few deaths
# thresh <- 1
# ratios$Ratio1[which(ratios$Deaths1.x < thresh | ratios$Deaths1.y < thresh)] <- .5
# ratios$Ratio2[which(ratios$Deaths2.x < thresh | ratios$Deaths2.y < thresh)] <- .5
# ratios$RQwom[which(ratios$Deaths2.x < thresh | ratios$Deaths2.y < thresh)] <- 1
# ratios$RQmen[which(ratios$Deaths2.x < thresh | ratios$Deaths2.y < thresh)] <- 1
# 
# # Crisis deaths
# ratios$crisisWom <- F
# ratios$crisisWom[which(ratios$Deaths2.x - ratios$Deaths1.x > 0)] <- T
# ratios$crisisMen <- F
# ratios$crisisMen[which(ratios$Deaths2.y - ratios$Deaths1.y > 0)] <- T
# 
# # Tidy up
# ratios <- ratios[, c("iso3", "year", "Ratio1", "Ratio2", 
#                      "RQwom", "RQmen", "crisisWom", "crisisMen")]
# 
# 
# 
# # For each country
# for (iso in 1:dim(deaths1Wom)[1]) {
#   
#   # For each year
#   for (year in 1:dim(deaths1Wom)[2]) {
#     
#     # Threshold
#     thresh2 <- 0
#     
#     # Proportions/Ratios
#     ratio1 <- ratios$Ratio1[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]
#     ratio2 <- ratios$Ratio2[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]
#     rQwom <- ratios$RQwom[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]
#     rQmen <- ratios$RQmen[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]
#     
#     # Female deaths (crisis-included)
#     if (any(is.na(deaths2Wom[iso, year, ]))) {
#       idna <- which(is.na(deaths2Wom[iso, year, ]))
#       if (length(idna) < thresh2) {
#         deaths2Wom[iso, year, idna] <- 
#           round(rnorm(n = length(idna),
#                       mean = mean(deaths2Wom[iso, year, ], na.rm = T),
#                       sd = sd(deaths2Wom[iso, year, ], na.rm = T)))
#       } else deaths2Wom[iso, year, idna] <- round(deaths2[iso, year, idna] * ratio2)
#     }
#     
#     # Female deaths (crisis-free)
#     if (any(is.na(deaths1Wom[iso, year, ]))) {
#       # Crisis deaths on that year?
#       if (ratios$crisisWom[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]) {
#         idna <- which(is.na(deaths1Wom[iso, year, ]))
#         # If below threshold, assign values randomly
#         if (length(idna) < thresh2) {
#           deaths1Wom[iso, year, idna] <- 
#             round(rnorm(n = length(idna),
#                         mean = mean(deaths1Wom[iso, year, ], na.rm = T),
#                         sd = sd(deaths1Wom[iso, year, ], na.rm = T)))
#         } else deaths1Wom[iso, year, idna] <- round(deaths1[iso, year, idna] * ratio1)
#       } else deaths1Wom[iso, year, idna] <- deaths2Wom[iso, year, idna]
#     }
#     
#     # Male deaths (crisis-included)
#     if (any(is.na(deaths2Men[iso, year, ]))) {
#       idna <- which(is.na(deaths2Men[iso, year, ]))
#       if (length(idna) < thresh2) {
#         deaths2Men[iso, year, idna] <- 
#           round(rnorm(n = length(idna),
#                       mean = mean(deaths2Men[iso, year, ], na.rm = T),
#                       sd = sd(deaths2Men[iso, year, ], na.rm = T)))
#       } else deaths2Men[iso, year, idna] <- round(deaths2[iso, year, idna] * (1 - ratio2))
#     }
#     
#     # Male deaths (crisis-free)
#     if (any(is.na(deaths1Men[iso, year, ]))) {
#       # Crisis deaths on that year?
#       if (ratios$crisisMen[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]) {
#         idna <- which(is.na(deaths1Men[iso, year, ]))
#         # If below threshold, assign values randomly
#         if (length(idna) < thresh2) {
#           deaths1Men[iso, year, idna] <- 
#             round(rnorm(n = length(idna),
#                         mean = mean(deaths1Men[iso, year, ], na.rm = T),
#                         sd = sd(deaths1Men[iso, year, ], na.rm = T)))
#         } else deaths1Men[iso, year, idna] <- round(deaths1[iso, year, idna] * (1 - ratio1))
#       } else deaths1Men[iso, year, idna] <- deaths2Men[iso, year, idna]
#     }
#     
#     # Female rates
#     if (any(is.na(rates2Wom[iso, year, ]))) {
#       idna <- which(is.na(rates2Wom[iso, year, ]))
#       rates2[iso, year, idna] <- rates2[iso, year, idna] * rQwom
#     }
#     
#     # Male rates
#     if (any(is.na(rates2Men[iso, year, ]))) {
#       idna <- which(is.na(rates2Men[iso, year, ]))
#       rates2Men[iso, year, idna] <- rates2[iso, year, idna] * rQmen
#     }
#     
#   }
#   
# }
# rm(iso, men, wom, ratios, ratioQ, ratio1, ratio2, rQmen, rQwom, year)

# Temporary: drop draws with NA values for either sex ---------------

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
