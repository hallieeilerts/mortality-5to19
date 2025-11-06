################################################################################
#' @description Create regional crisis-included envelopes for 5-9, 10-14, 15-19m, 15-19f, 15-19 combined
#' @return Data frame with c(iso3, year, Deaths2, Rate2)
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
## envelope
env <- read.csv("./gen/data-management/output/env_u20.csv")
## Classification keys
key_agesexgrp <- read.csv("./gen/data-management/output/key_agesexgrp_u20.csv")
key_region <- read.csv("./gen/data-management/output/key_region_u20.csv")
################################################################################

# Merge regions onto national envelopes
dat <- merge(env, key_region, by = "iso3")

dat <- subset(dat, AgeSexSuffix %in% c("05to09y", "10to14y", "15to19yF", "15to19yM", "15to19y"))

# Back calculate denominator from deaths and mortality rate
dat$Px <- dat$Deaths2/dat$Rate2

if(ageSexSuffix %in% c("05to09y", "10to14y")){
  # Aggregate deaths and denominators for countries for each region
  dat_list <- lapply(split(dat, dat$Region), function(x) {
    tmp <- aggregate(x[, c("Deaths1", "Deaths2", "Px")], 
                     by = list(year = x$year), 
                     sum, na.rm = TRUE)
    tmp$Region <- unique(x$Region)
    tmp$AgeSexSuffix <- ageSexSuffix
    return(tmp)
  })
  dat <- do.call(rbind, dat_list)
  
  # Aggregate for world
  dat_world <- aggregate(dat[, c("Deaths1", "Deaths2", "Px")], 
                         by = list(year = dat$year), 
                         sum, na.rm = TRUE)
  dat_world$Region <- "World"
  dat_world$AgeSexSuffix <- ageSexSuffix
  
  # Combine
  dat <- rbind(dat_world, dat)
}
if(ageSexSuffix %in% c("15to19yF", "15to19yM", "15to19y")){
  
  v_15to19 <- c("15to19yF", "15to19yM", "15to19y")
  dat15to19 <- data.frame()
  
  for(i in 1:length(v_15to19)){
    
    my15to19 <- v_15to19[i]
    dat15to19i <- subset(dat, AgeSexSuffix %in% my15to19)
    
    # Aggregate deaths and denominators for countries for each region
    dat_list <- lapply(split(dat15to19i, dat15to19i$Region), function(x) {
      tmp <- aggregate(x[, c("Deaths1", "Deaths2", "Px")], 
                       by = list(year = x$year), 
                       sum, na.rm = TRUE)
      tmp$Region <- unique(x$Region)
      tmp$AgeSexSuffix <- my15to19
      tmp
    })
    dati <- do.call(rbind, dat_list)
    
    # Aggregate for world
    dat_worldi <- aggregate(dati[, c("Deaths1", "Deaths2", "Px")], 
                           by = list(year = dati$year), 
                           sum, na.rm = TRUE)
    dat_worldi$Region <- "World"
    dat_worldi$AgeSexSuffix <- my15to19
    
    dat15to19 <- rbind(dat15to19, dat_worldi, dati)
  }
  
  dat <- dat15to19
}

# Re-calculate mortality rate
dat$Rate1 <- dat$Deaths1 / dat$Px
dat$Rate2 <- dat$Deaths2 / dat$Px
dat$Px <- NULL

# Tidy
dat <- dat[,c("AgeSexSuffix", "Region", "year", "Deaths1", "Deaths2", "Rate1", "Rate2")]
row.names(dat) <- NULL

# Remove AgeSexSuffix column
if(ageSexSuffix %in% c("05to09y","10to14y")){
  dat$AgeSexSuffix <- NULL
}
if(ageSexSuffix %in% c("15to19yF", "15to19yM")){
  dat_15to19f <- subset(dat, AgeSexSuffix == "15to19yF")
  dat_15to19m <- subset(dat, AgeSexSuffix == "15to19yM")
  dat_15to19 <- subset(dat, AgeSexSuffix == "15to19y")
  dat_15to19f$AgeSexSuffix <- NULL
  dat_15to19m$AgeSexSuffix <- NULL
  dat_15to19$AgeSexSuffix <- NULL
}

# Save output(s) ----------------------------------------------------------

if(ageSexSuffix %in% c("05to09y","10to14y")){
  write.csv(dat, paste("./gen/data-management/output/env_",ageSexSuffix,"_REG.csv", sep = ""), row.names = FALSE)
}
if(ageSexSuffix %in% c("15to19yF", "15to19yM")){
  write.csv(dat_15to19f, paste("./gen/data-management/output/env_15to19yF_REG.csv", sep = ""), row.names = FALSE)
  write.csv(dat_15to19m, paste("./gen/data-management/output/env_15to19yM_REG.csv", sep = ""), row.names = FALSE)
  write.csv(dat_15to19, paste("./gen/data-management/output/env_15to19y_REG.csv", sep = ""), row.names = FALSE)
}

