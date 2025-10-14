################################################################################
#' @description Updates variable names of China DSP data
#' @return Data frame with China CSMFs with updated variable names.
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(readstata13)
library(tidyr)
library(dplyr)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat <- read.dta13("./data/china/20210330-ChinaDSP.dta", nonint.factors = T)
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageSexSuffix, ".csv", sep=""))
key_codlist <- read.csv(paste("./gen/data-management/output/key_codlist_", ageSexSuffix, ".csv", sep=""))
################################################################################

# Add country variable
dat$iso3 <- "CHN"

# Create new age and sex variables
dat$AgeSexSuffix <- "05to09y"
dat$AgeSexSuffix[dat$group == "Both 10-14"] <- "10to14y"
dat$AgeSexSuffix[dat$group == "Female 15-19_(4)"] <- "15to19yF"
dat$AgeSexSuffix[dat$group == "Male 15-19"] <- "15to19yM"

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

### !!!! NOTE - this will be different for different age groups
# need to improve how this is done

# Alter reclassification key
# Still want to reclassify, but the squeezing is different for China, so need to alter how this is done
# Only HIV gets squeezed in for China, so retain any other reported causes that are missing
# Identify which reported COD are not in the reclass key (i.e., the are reclassified to another cause)
v_cod_reported <- subset(key_codlist, ModeledOrReported == "Reported")$COD
v_dont_reclass <- v_cod_reported[!(v_cod_reported %in% key_cod$cod_reclass)]
v_dont_reclass # "CollectVio" "NatDis"   "Measles"  "TB"  "HIV"
# drop HIV, because actually do want to reclassify this. this gets squeezed in for China.
v_dont_reclass <- v_dont_reclass[!(v_dont_reclass %in% "HIV")]
v_dont_reclass <- v_dont_reclass[order(v_dont_reclass)]
# Alter reclass key with causes that should be retained (formerly were reclassified to a different cause)
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
# key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "colvio"] <- v_dont_reclass[1]
# key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "mea"] <- v_dont_reclass[2]
# key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "natdis"] <- v_dont_reclass[3]
# Drop causes that are dropped from data
key_cod_chn <- subset(key_cod_chn, !is.na(cod_reclass))

# Vector with China-specific reclass categories (including single-cause estimates)
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
dat <- dat[, names(dat) %in% c(idVars, v_reclass)]

# Extrapolate fractions
dat <- dat %>% arrange(year, iso3) %>%
  complete(year = Years,
           iso3 = "CHN") %>%
  fill(all_of(v_reclass), .direction = "down")

# v_orig <- c("dia","mea","mening","lri","tb","maternal","othercmpn","congen","neoplasm","cardio","endo","digest","otherncd","rta","drown","natdis","intvio","colvio","selfharm","injuries")
# v_orig[!(v_orig %in% key_cod$cod_mapped)]
# 
# # # Add missing categories to match VA COD list
# # if (!"typhoid" %in% names(dat)) dat$typhoid <- 0
# # if (!"other" %in% names(dat)) dat$other <- 0
# # if (!"undt" %in% names(dat)) dat$undt <- 0
# # if (!"hiv" %in% names(dat)) dat$hiv <- 0
# # if (!"mal" %in% names(dat)) dat$mal <- 0
# 
# #### NOTE
# # NEED TO MAKE SURE MALARIA AND MEASLES ARE INCLUDED!!! IMPORTANT FOR SQUEEZING sqzcrisisepi later
# 
# 
# 
# # Create new key that only includes causes in china
# key_cod_chn <- subset(key_cod, cod_mapped %in% v_orig)
# # Alter reclassification key to match reported causes
# # Still want to reclassify, but also want to retain causes which ultimately get reported
# key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "colvio"] <- "CollectVio"
# key_cod_chn$cod_reclass[key_cod_chn$cod_mapped == "natdis"] <- "NatDis"
# 
# # Vector with all CODs (including single-cause estimates)
# v_cod <- unique(key_cod_chn$cod_reclass)
# v_cod <- v_cod[!is.na(v_cod)]
# v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
# # For China, also exclude HIV as this will be added through squeezing
# v_cod <- v_cod[v_cod != "HIV"]
# 
# # NOTE: CHECK ORDERING HERE. IS THIS HOW I WANT TO TREAT NATDIS AND COLVIO?
# 
# # Re-classify causes of death
# for(i in 1:length(v_cod)){
#   
#   orig <- key_cod_chn$cod_mapped[key_cod_chn$cod_reclass == v_cod[i]]
#   if (length(orig) > 1) {
#     dat[, paste(v_cod[i])] <- apply(dat[, paste(orig)], 1, 
#                                     function(x) {
#                                       if (all(is.na(x))) {
#                                         return(NA)
#                                       } else return(sum(x, na.rm = T))
#                                     })
#   } else dat[, paste(v_cod[i])] <- dat[, paste(orig)]
# }
# 
# # Select idvars and COD columns
# dat <- dat[, names(dat) %in% c(idVars, v_cod)]
# 
# # Extrapolate fractions
# dat <- dat %>% arrange(year, iso3) %>%
#   complete(year = 2000:2024,
#            iso3 = "CHN") %>%
#   fill(any_of(v_cod), .direction = "down")

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/data-management/output/csmf_ChinaDSP_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)

