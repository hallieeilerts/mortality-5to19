################################################################################
#' @description Rename columns
#' @return Data frame with c("ISO3", "Year", "dth_malaria_5to19")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
require(tidyr)
require(dplyr)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
csmf_01to04 <- read.dta13("./data/mortality-fractions/child_cod_2000-2021.dta")
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# Set age/sex groups contained in csmf data
v_AgeSexLabel <- c("Months1to59")

# Clean data --------------------------------------------------------------

dat <- csmf_01to04

# Select countries (exclude regions)
dat <- subset(dat, !is.na(iso3))
dat <- subset(dat, iso3 != "NA")
# Refer to "child_cod_2000-2021.xls" readme tab to identify column name for postneonatal malaria CSMF
dat$csmf_malaria_01to04 <- dat$post8/dat$pnd
dat$csmf_malaria_01to04[is.na(dat$csmf_malaria_01to04)] <- 0
dat <- dat[,c("iso3", "year", "csmf_malaria_01to04")]

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$iso3) %in% dat$iso3)) > 0){
  warning("Not all countries included in data input.")
  write.table(sort(unique(key_ctryclass_u20$WHOname)[!(unique(key_ctryclass_u20$iso3) %in% dat$iso3)]), 
              "./gen/data-management/audit/missing_mal-frac-cap.txt")
}

# Fill in zeros for missing country-years, if necessary

# Create data frame for countries/years of interest
# For malaria fraction cap data, HMM countries
df_ctryyears <- data.frame(iso3 = rep(key_ctryclass_u20$iso3, each = length(Years)),
                           year = rep(Years))

# Merge onto COD data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars[1:2], all = TRUE)

# Add agesexgrp
dat$AgeSexLabel <- v_AgeSexLabel[1]

# Fill missing values -----------------------------------------------------

# Recode missing csmf 0
dat$csmf_malaria_01to04[which(is.na(dat$csmf_malaria_01to04) & dat$year <= 2021)] <- 0

# Extrapolate from 2021 to 2023
extend <- dat %>% 
  arrange(AgeSexLabel, iso3, year) %>%
  filter(year >= 2021) %>% 
  group_by(AgeSexLabel, iso3) %>%
  fill(csmf_malaria_01to04, .direction = "down") %>% 
  filter(year > 2021)

# Rbind extrapolated data
dat <- dat %>% filter(year <= 2021) %>%
  rbind(., extend)

# Tidy up
dat <- dat[, c("iso3", "year", "csmf_malaria_01to04")]
dat <- dat[order(dat$iso3, dat$year),]
rownames(dat) <- NULL

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/input/frac_malaria_01to04.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
