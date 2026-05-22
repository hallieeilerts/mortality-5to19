################################################################################
#' @description Updates variable names of China DSP data
#' @return Data frame with China CSMFs with updated variable names
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(readstata13)
library(tidyr)
library(dplyr)
#' Inputs
source("./src/prepare-session/set-inputs.R")
dat <- read.dta13("./data/china/20210330-ChinaDSP.dta", nonint.factors = T)
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageSexSuffix, ".csv", sep=""))
key_codlist <- read.csv(paste("./gen/data-management/output/key_codlist_", ageSexSuffix, ".csv", sep=""))
key_agesexgrp <- read.csv("./gen/data-management/output/key_agesexgrp_u20.csv")
################################################################################

# Add country variable
dat$iso3 <- "CHN"

# Create new age and sex variables
dat$AgeSexSuffix <- subset(key_agesexgrp, AgeSexLabel == "Years5to9")$AgeSexSuffix
dat$AgeSexSuffix[dat$group == "Both 10-14"] <- subset(key_agesexgrp, AgeSexLabel == "Years10to14")$AgeSexSuffix
dat$AgeSexSuffix[dat$group == "Female 15-19_(4)"] <- subset(key_agesexgrp, AgeSexLabel == "Years15to19f")$AgeSexSuffix
dat$AgeSexSuffix[dat$group == "Male 15-19"] <- subset(key_agesexgrp, AgeSexLabel == "Years15to19m")$AgeSexSuffix

# Keep age group of interest
dat <- subset(dat, AgeSexSuffix == ageSexSuffix)

# Re-label variables
names(dat)[names(dat) == "csdf3"] <- "dia"
names(dat)[names(dat) == "csdf4"] <- "mea"
names(dat)[names(dat) == "csdf7"] <- "mening"
names(dat)[names(dat) == "csdf9"] <- "lri"
names(dat)[names(dat) == "csdf10"] <- "tb"
names(dat)[names(dat) == "csdf11"] <- "maternal"
names(dat)[names(dat) == "csdf12"] <- "othercmpn"
names(dat)[names(dat) == "csdf14"] <- "congen"
names(dat)[names(dat) == "csdf15"] <- "neoplasm"
names(dat)[names(dat) == "csdf16"] <- "cardio"
names(dat)[names(dat) == "csdf17"] <- "endo"
names(dat)[names(dat) == "csdf18"] <- "digest"
names(dat)[names(dat) == "csdf19"] <- "otherncd"
names(dat)[names(dat) == "csdf21"] <- "rta"
names(dat)[names(dat) == "csdf22"] <- "drown"
names(dat)[names(dat) == "csdf23"] <- "natdis"
names(dat)[names(dat) == "csdf24"] <- "intvio"
names(dat)[names(dat) == "csdf25"] <- "colvio"
names(dat)[names(dat) == "csdf27"] <- "selfharm"
names(dat)[names(dat) == "csdf28"] <- "injuries"

# Add zero for causes that are missing from reclassification key
v_add <- key_cod$cod_mapped[!(key_cod$cod_mapped %in% names(dat))]
for(i in 1:length(v_add)){
  dat$new <- 0
  names(dat)[names(dat) %in% "new"] <- v_add[i]
}

# Alter reclassification key
# Still necessary to reclassify CODs for China, but the CODs that get reclassified are slightly different due to different squeezing methods.
# (Only HIV gets squeezed in)

# Identify which reported COD are not in the reclass column
# These COD are are reclassified as an "other" cause.
v_cod_reported <- subset(key_codlist, ModeledOrReported == "Reported")$COD
# We do not want to reclassify them for China like we do for HMM countries
v_dont_reclass <- v_cod_reported[!(v_cod_reported %in% key_cod$cod_reclass)]
v_dont_reclass # "CollectVio" "NatDis" "Measles" "TB" "HIV"

# Remove HIV from this vector
# We do want to reclassify HIV for China, because it will be squeezed in.
v_dont_reclass <- v_dont_reclass[!(v_dont_reclass %in% "HIV")]
v_dont_reclass <- v_dont_reclass[order(v_dont_reclass)]

# Create a new COD key that is China-specific

# Alter reclass column for CODs that should be retained (formerly were reclassified to an "other" cause or dropped)
key_cod_chn <- key_cod
if("CollectVio" %in% v_dont_reclass){
  key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "colvio"] <- "CollectVio"
}
if("Measles" %in% v_dont_reclass){
  key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "mea"] <- "Measles"
}
if("TB" %in% v_dont_reclass){
  key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "tb"] <- "TB"
}
if("NatDis" %in% v_dont_reclass){
  key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "natdis"] <- "NatDis"
}
# Drop causes that are dropped from data
key_cod_chn <- subset(key_cod_chn, !is.na(cod_reclass))

# Vector with China-specific reclass categories
v_reclass <- unique(key_cod_chn$cod_reclass)
# Drop causes that should be reclassified to nothing (dropped)
v_reclass <- v_reclass[!(v_reclass %in% c("Other", "Undetermined"))]

# Re-classify causes of death
for(i in 1:length(v_reclass)){
  
  orig <- key_cod_chn$cod_mapped[key_cod_chn$cod_reclass == v_reclass[i]]
  if (length(orig) > 1) {
    dat[, paste(v_reclass[i])] <- apply(dat[, paste(orig)], 1, 
                                    function(x) {
                                      if (all(is.na(x))) {
                                        return(NA)
                                      } else return(sum(x, na.rm = T))
                                    })
  } else dat[, paste(v_reclass[i])] <- dat[, paste(orig)]
}

# Select idvars and COD columns
dat <- dat[, names(dat) %in% c("iso3", "year", v_reclass)]

# Extrapolate fractions
dat <- dat %>% arrange(year, iso3) %>%
  complete(year = Years,
           iso3 = "CHN") %>%
  fill(all_of(v_reclass), .direction = "down")

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/data-management/output/csmf_ChinaDSP_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)

