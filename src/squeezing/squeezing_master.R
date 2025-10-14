################################################
# Squeezing
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/squeezing/squeezing_inputs.R")
source("./src/squeezing/squeezing_functions.R")

# Merge on envelopes
csmf_envADD        <- fn_mergeEnv(csmf, env)
csmf_envADD_CHN    <- fn_mergeEnv(csmf_CHN, env)
csmf_envADD_GOODVR <- fn_mergeEnv(csmf_GOODVR, env)

# Prepare modeled countries and China for squeezing
csmf_singlecauseADD <- fn_prepareSqz(csmf_envADD, dat_tb, dat_hiv, dat_crisis, dat_meas, frac_cd, frac_lri)
csmf_singlecauseADD_CHN <- fn_prepareSqzChina(csmf_envADD_CHN, dat_hiv, dat_crisis, frac_cd)

# Perform squeezing
if(ageSexSuffix == "05to09y"){
  csmf_othercmpnSQZ     <- fn_sqzOtherCMPN(csmf_singlecauseADD)
  csmf_lriSQZ           <- fn_sqzLRI(csmf_othercmpnSQZ)
  csmf_crisisEndSQZ     <- fn_sqzCrisisEnd(csmf_lriSQZ, key_codlist)
  dth_crisisEpiSQZ      <- fn_sqzCrisisEpi(csmf_crisisEndSQZ, key_codlist)
  dth_measEpiADD        <- fn_addMeasEpi(dth_crisisEpiSQZ)
  dth_SQZ <- dth_measEpiADD
  csmf_othercmpnSQZ_CHN <- fn_sqzOtherCMPNchina(csmf_singlecauseADD_CHN)
  dth_crisisEpiSQZ_CHN  <- fn_sqzCrisisEpi(csmf_othercmpnSQZ_CHN, key_codlist)
  dth_SQZ_CHN <- dth_crisisEpiSQZ_CHN
}
if(ageSexSuffix == "10to14y"){
  csmf_othercmpnSQZ     <- fn_sqzOtherCMPN(csmf_singlecauseADD)
  csmf_lriSQZ           <- fn_sqzLRI(csmf_othercmpnSQZ)
  csmf_crisisEndSQZ     <- fn_sqzCrisisEnd(csmf_lriSQZ, key_codlist) # fn_sqzCrisisEnd_old(csmf_lriSQZ, key_codlist)
  dth_crisisEpiSQZ      <- fn_sqzCrisisEpi(csmf_crisisEndSQZ, key_codlist) # fn_sqzCrisisEpi_old(csmf_crisisEndSQZ, key_codlist)
  dth_SQZ <- dth_crisisEpiSQZ
  csmf_othercmpnSQZ_CHN <- fn_sqzOtherCMPNchina(csmf_singlecauseADD_CHN)
  dth_crisisEpiSQZ_CHN  <- fn_sqzCrisisEpi(csmf_othercmpnSQZ_CHN, key_codlist) # fn_sqzCrisisEpi_old(csmf_othercmpnSQZ_CHN, key_codlist)
  dth_SQZ_CHN <- dth_crisisEpiSQZ_CHN
}
if(ageSexSuffix %in% c("15to19yF", "15to19yM")){
  csmf_othercmpnSQZ     <- fn_sqzOtherCMPN(csmf_singlecauseADD)
  csmf_crisisEndSQZ     <- fn_sqzCrisisEnd(csmf_othercmpnSQZ, key_codlist)
  dth_crisisEpiSQZ      <- fn_sqzCrisisEpi(csmf_crisisEndSQZ, key_codlist)
  dth_SQZ <- dth_crisisEpiSQZ
  csmf_othercmpnSQZ_CHN <- fn_sqzOtherCMPNchina(csmf_singlecauseADD_CHN)
  dth_crisisEpiSQZ_CHN  <- fn_sqzCrisisEpi(csmf_othercmpnSQZ_CHN, key_codlist)
  dth_SQZ_CHN <- dth_crisisEpiSQZ_CHN
}

# Format squeezed output
csmf_SQZ <- fn_formatSqzOutput(dth_SQZ, dth_SQZ_CHN, csmf_envADD, csmf_envADD_CHN, key_codlist)

# Audit: check if squeezed CSMFs add up to 1 or contain NA
csmf_SQZ_AUD <- fn_checkCSMFsqz(csmf_SQZ, key_codlist)
if(nrow(csmf_SQZ_AUD) > 0){
  write.csv(csmf_SQZ_AUD, paste("./gen/squeezing/audit/csmf_SQZ_AUD_", ageSexSuffix,".csv", sep=""), row.names = FALSE)
}
# For 5-9y, 10-14, 15-19yf, only happens when Deaths1 == 0
nrow(subset(csmf_SQZ_AUD, Deaths1 != 0)) # 0
# One case of 15-19ym when csmf sum is 0.999 (China 2005)

# Combine squeezed output from modeled countries (HMM and LMM) and China with GOODVR, format, save
csmfSqz <- rbind(csmf_SQZ, csmf_envADD_GOODVR)
csmfSqz <- fn_formatAllOutput(csmfSqz, key_codlist)
write.csv(csmfSqz, paste("./gen/squeezing/output/csmfSqz_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)

# Calculate regional CSMFs
env_REG$year <- env_REG$Year 
env_REG$Year <- NULL
csmfSqz_REG <- fn_calcRegion(csmfSqz, env_REG, codAll, key_region)
# BARCELONA MEETING: REMOVES REGIONS IF NOT ALL COUNTRIES IN REGION REPORTED
v_remove <- c()
for(i in 1:length(unique(csmfSqz_REG$Region))){
  datAux <- merge(csmfSqz, key_region, by = "iso3")  
  datAux <- subset(datAux, Region == unique(csmfSqz_REG$Region)[i])
  keyAux <- subset(key_region, Region == unique(csmfSqz_REG$Region)[i])
  if(!all(keyAux$iso3 %in% unique(datAux$iso3))){
    v_remove <- c(v_remove, unique(csmfSqz_REG$Region)[i])
  }
}
if(length(v_remove) > 0){
  csmfSqz_REG <- subset(csmfSqz_REG, !(Region %in% v_remove))
}
write.csv(csmfSqz_REG, paste("./gen/squeezing/output/csmfSqz_", ageSexSuffix, "REG.csv", sep=""), row.names = FALSE)
