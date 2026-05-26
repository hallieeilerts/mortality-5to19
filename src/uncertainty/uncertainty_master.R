################################################
# Uncertainty
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/uncertainty/uncertainty_inputs.R")
source("./src/prediction/prediction_inputs.R")
source("./src/squeezing/squeezing_inputs.R")

# Remove unnecessary inputs from prediction
rm(dat_hp, dat_pred, key_ctryclass, cases_malaria_05to19, frac_malaria_01to04)
detach("package:MASS")

# Prepare draws -----------------------------------------------------------

# Create sampling vectors for IGME envelope draws based on number of CSMF draws
# This will ensure that there are the same number of draws from each source
v_sample <- fn_createSampleVectors(csmfDraws, envDraws)
# Save temporary file for use in calculating aggregate age groups
saveRDS(v_sample, file = paste("./gen/uncertainty/temp/sampleDraws.rds", sep=""))

# Sample from all draws
envDraws_SAMP <- fn_randDrawEnv(envDraws, v_sample$env)

# Prepare squeezing -------------------------------------------------------

# Merge on envelopes
csmfDraws_envADD        <- mapply(function(x,y) fn_mergeEnv(x,y), csmfDraws, envDraws_SAMP, SIMPLIFY = FALSE)
csmfList_envADD_GOODVR  <- lapply(envDraws_SAMP, function(x){ fn_mergeEnv(csmf_GOODVR, x)})
csmfList_envADD_CHN     <- lapply(envDraws_SAMP, function(x){ fn_mergeEnv(csmf_CHN, x)})

# Randomly assign CSMFs for VR/China for each draw
csmfDraws_GOODVR <- lapply(csmfList_envADD_GOODVR, function(x){ fn_randAssignVR(x, key_codlist, CTRYGRP = "GOODVR")})
csmfDraws_CHN    <- lapply(csmfList_envADD_CHN, function(x){ fn_randAssignVR(x, key_codlist, CTRYGRP = "CHN")})

# Prepare modeled countries and China for squeezing
csmfDraws_singlecauseADD <- lapply(csmfDraws_envADD, function(x) fn_prepareSqz(x, dat_tb, dat_hiv, dat_crisis, dat_meas, frac_cd, frac_lri))
csmfDraws_singlecauseADD_CHN <- lapply(csmfDraws_CHN, function(x) fn_prepareSqzChina(x, dat_hiv, dat_crisis, frac_cd))

# Randomly assign single causes for each draw
if(ageSexSuffix == "05to09y"){
  csmfDraws_singlecauseADD <- lapply(csmfDraws_singlecauseADD, function(x){ fn_randAssignMeas(x) })
}
csmfDraws_singlecauseADD <- lapply(csmfDraws_singlecauseADD, function(x){ fn_randAssignTB(x) })
csmfDraws_singlecauseADD <- lapply(csmfDraws_singlecauseADD, function(x){ fn_randAssignHIV(x) })
csmfDraws_singlecauseADD_CHN <- lapply(csmfDraws_singlecauseADD_CHN, function(x){ fn_randAssignHIV(x) })

# Squeezing ---------------------------------------------------------------

if(ageSexSuffix == "05to09y"){
  csmfDraws_othercmpnSQZ     <- lapply(csmfDraws_singlecauseADD,     function(x){ fn_sqzOtherCMPN(x) })
  csmfDraws_lriSQZ           <- lapply(csmfDraws_othercmpnSQZ,       function(x){ fn_sqzLRI(x) })
  csmfDraws_crisisEndSQZ     <- lapply(csmfDraws_lriSQZ,             function(x){ fn_sqzCrisisEnd(x, key_codlist, UNCERTAINTY = TRUE) })
  dthDraws_crisisEpiSQZ      <- lapply(csmfDraws_crisisEndSQZ,       function(x){ fn_sqzCrisisEpi(x, key_codlist) })
  dthDraws_measEpiADD        <- lapply(dthDraws_crisisEpiSQZ,        function(x){ fn_addMeasEpi(x) })
  dthDraws_SQZ               <- dthDraws_measEpiADD
  csmfDraws_othercmpnSQZ_CHN <- lapply(csmfDraws_singlecauseADD_CHN, function(x){ fn_sqzOtherCMPNchina(x) })
  dthDraws_crisisEpiSQZ_CHN  <- lapply(csmfDraws_othercmpnSQZ_CHN,   function(x){ fn_sqzCrisisEpi(x, key_codlist) })
  dthDraws_SQZ_CHN <- dthDraws_crisisEpiSQZ_CHN
}
if(ageSexSuffix == "10to14y"){
  csmfDraws_othercmpnSQZ     <- lapply(csmfDraws_singlecauseADD,     function(x){ fn_sqzOtherCMPN(x) })
  csmfDraws_lriSQZ           <- lapply(csmfDraws_othercmpnSQZ,       function(x){ fn_sqzLRI(x) })
  csmfDraws_crisisEndSQZ     <- lapply(csmfDraws_lriSQZ,             function(x){ fn_sqzCrisisEnd(x, key_codlist, UNCERTAINTY = TRUE) })
  dthDraws_crisisEpiSQZ      <- lapply(csmfDraws_crisisEndSQZ,       function(x){ fn_sqzCrisisEpi(x, key_codlist) })
  dthDraws_SQZ               <- dthDraws_crisisEpiSQZ
  csmfDraws_othercmpnSQZ_CHN <- lapply(csmfDraws_singlecauseADD_CHN, function(x){ fn_sqzOtherCMPNchina(x) })
  dthDraws_crisisEpiSQZ_CHN  <- lapply(csmfDraws_othercmpnSQZ_CHN,   function(x){ fn_sqzCrisisEpi(x, key_codlist) })
  dthDraws_SQZ_CHN <- dthDraws_crisisEpiSQZ_CHN
}
if(ageSexSuffix %in% c("15to19yF", "15to19yM")){
  csmfDraws_othercmpnSQZ     <- lapply(csmfDraws_singlecauseADD,     function(x){ fn_sqzOtherCMPN(x) })
  csmfDraws_crisisEndSQZ     <- lapply(csmfDraws_othercmpnSQZ,       function(x){ fn_sqzCrisisEnd(x, key_codlist, UNCERTAINTY = TRUE) })
  dthDraws_crisisEpiSQZ      <- lapply(csmfDraws_crisisEndSQZ,       function(x){ fn_sqzCrisisEpi(x, key_codlist) })
  dthDraws_SQZ               <- dthDraws_crisisEpiSQZ
  csmfDraws_othercmpnSQZ_CHN <- lapply(csmfDraws_singlecauseADD_CHN, function(x){ fn_sqzOtherCMPNchina(x) })
  dthDraws_crisisEpiSQZ_CHN  <- lapply(csmfDraws_othercmpnSQZ_CHN,   function(x){ fn_sqzCrisisEpi(x, key_codlist) })
  dthDraws_SQZ_CHN <- dthDraws_crisisEpiSQZ_CHN
}

# Remove unnecessary objects
suppressWarnings(rm(csmfDraws_SAMP, envDraws_SAMP,
                    csmfDraws_othercmpnSQZ, csmfDraws_lriSQZ, csmfDraws_crisisEndSQZ,
                    dthDraws_crisisEpiSQZ, dthDraws_measEpiADD,
                    csmfDraws_othercmpnSQZ_CHN, dthDraws_crisisEpiSQZ_CHN))

# Format squeezed output
csmfDraws_SQZ <- mapply(function(w,x,y,z) fn_formatSqzOutput(w,x,y,z, key_codlist), 
                        dthDraws_SQZ, dthDraws_SQZ_CHN, csmfDraws_envADD, csmfDraws_CHN, SIMPLIFY = FALSE)

# Audit: check if squeezed CSMFs add up to 1 or contain NA
csmfDraws_SQZ_AUD  <- lapply(csmfDraws_SQZ, function(x){ fn_checkCSMFsqz(x, key_codlist) })
df_csmfDraws_SQZ_AUD <- plyr::ldply(csmfDraws_SQZ_AUD, .id = "draw")
nrow(df_csmfDraws_SQZ_AUD) # 0, 11 for 15-19m but they round to 1
if(nrow(df_csmfDraws_SQZ_AUD) > 0){
  write.csv(df_csmfDraws_SQZ_AUD, paste("./gen/uncertainty/audit/csmfDraws_SQZ_AUD_", ageSexSuffix,".csv", sep=""), row.names = FALSE)
}

# Combine squeezed output from modeled countries and China with GOODVR, format, save
csmfDraws_All <- mapply(rbind, csmfDraws_SQZ, csmfDraws_GOODVR, SIMPLIFY = FALSE)
csmfDraws_All <- lapply(csmfDraws_All, function(x){ fn_formatAllOutput(x, key_codlist) })
## saveRDS(csmfDraws_All, paste0("./gen/uncertainty/output/csmfSqzDraws_", ageSexSuffix, ".rds"))

# Calculate regional CSMFs
csmfSqzDraws_REG <- lapply(csmfDraws_SQZ, function(x){ fn_calcRegion(x,  ENV_REGION = env_REG, KEY_REGION = key_region, KEY_CODLIST = key_codlist) })

# Remove unnecessary objects
rm(dthDraws_SQZ, dthDraws_SQZ_CHN, csmfDraws_envADD)

# Uncertainty -------------------------------------------------------------

# Calculate uncertainty intervals
ui <- fn_calcUI(csmfDraws_All, UI = 0.95, key_codlist)
ui_REG <- fn_calcUI(csmfSqzDraws_REG, UI = 0.95, key_codlist, REGIONAL = TRUE)

# Combine point estimates with uncertainty intervals
pointInt <- fn_combineUIpoint(ui, csmfPoint, key_codlist)
pointInt_REG <- fn_combineUIpoint(ui_REG, csmfPoint_REG, key_codlist, REGIONAL = TRUE)

# Round point estimates with uncertainty intervals
pointInt_FRMT <- fn_roundPointInt(pointInt, key_codlist)
pointInt_FRMT_REG <- fn_roundPointInt(pointInt_REG, key_codlist, REGIONAL = TRUE)

# Audit: check if point estimates fall in uncertainty bounds
pointInt_AUD <- fn_checkUI(pointInt_FRMT, key_codlist)
pointInt_AUD_REG <- fn_checkUI(pointInt_FRMT_REG, key_codlist, REGIONAL = TRUE)
if(nrow(pointInt_AUD) > 0){write.csv(pointInt_AUD, paste("./gen/uncertainty/audit/pointInt_AUD_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)}
if(nrow(pointInt_AUD_REG) > 0){write.csv(pointInt_AUD_REG, paste("./gen/uncertainty/audit/pointInt_AUD_", ageGroup,"REG_", resDate, ".csv", sep=""), row.names = FALSE)}

# Adjust point estimates and uncertainty intervals
pointInt_ADJ <- fn_adjustPointIntZeroDeaths(pointInt_FRMT, key_codlist)

# Audit: check if point estimates fall in uncertainty bounds
pointIntAdj_AUD <- fn_checkUI(pointInt_ADJ, key_codlist)
if(nrow(pointIntAdj_AUD) > 0){
  write.csv(pointIntAdj_AUD, paste("./gen/uncertainty/audit/pointIntAdj_AUD_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
}

# Save
write.csv(pointInt_ADJ, paste("./gen/uncertainty/output/pointInt_", ageGroup,".csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_REG, paste("./gen/uncertainty/output/pointInt_", ageGroup,"REG.csv", sep=""), row.names = FALSE)

