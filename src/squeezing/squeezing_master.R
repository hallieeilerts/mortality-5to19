################################################
# Squeezing
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/squeezing/squeezing_inputs.R")

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
  csmf_crisisEndSQZ     <- fn_sqzCrisisEnd(csmf_lriSQZ, key_codlist)
  dth_crisisEpiSQZ      <- fn_sqzCrisisEpi(csmf_crisisEndSQZ, key_codlist)
  dth_SQZ <- dth_crisisEpiSQZ
  csmf_othercmpnSQZ_CHN <- fn_sqzOtherCMPNchina(csmf_singlecauseADD_CHN)
  dth_crisisEpiSQZ_CHN  <- fn_sqzCrisisEpi(csmf_othercmpnSQZ_CHN, key_codlist)
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
write.csv(csmf_SQZ_AUD, paste("./gen/squeezing/audit/csmf_SQZ_AUD_", ageSexSuffix,".csv", sep=""), row.names = FALSE)

# Combine squeezed output from modeled countries (HMM and LMM) and China with GOODVR, format, save
csmfSqz <- rbind(csmf_SQZ, csmf_envADD_GOODVR)
csmfSqz <- fn_formatAllOutput(csmfSqz, key_codlist)
write.csv(csmfSqz, paste("./gen/squeezing/output/csmfSqz_", ageSexSuffix, ".csv", sep=""), row.names = FALSE)

# Calculate regional CSMFs
csmfSqz_REG <- fn_calcRegion(CSMF = csmfSqz, ENV_REGION = NULL, KEY_REGION = key_region, KEY_CODLIST = key_codlist)
write.csv(csmfSqz_REG, paste("./gen/squeezing/output/csmfSqz_", ageSexSuffix, "_REG.csv", sep=""), row.names = FALSE)
