################################################
# Prediction
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/prediction/prediction_inputs.R")

# Load HMM and LMM model fit
mod_fit_HMM <- fn_loadModFit(ageSexSuffix, "HMM", dat_hp)
mod_fit_LMM <- fn_loadModFit(ageSexSuffix, "LMM", dat_hp)

# Extract data for prediction, predict, summarise
mod_mat_HMM <- fn_par(mod_fit_HMM)
mod_pred_HMM <- fn_pr2(mod_mat_HMM, dat_pred, "HMM", key_ctryclass)
csmf_HMM <- fn_reshapePr2(mod_pred_HMM)
mod_mat_LMM <- fn_par(mod_fit_LMM)
mod_pred_LMM <- fn_pr2(mod_mat_LMM, dat_pred, "LMM", key_ctryclass)
csmf_LMM <- fn_reshapePr2(mod_pred_LMM)

# Draws of point estimates for uncertainty calculations
csmfDraws_HMM <- fn_reshapePr2(mod_pred_HMM, UNCERTAINTY = TRUE)
csmfDraws_LMM <- fn_reshapePr2(mod_pred_LMM, UNCERTAINTY = TRUE)

# Set malaria fractions
if(ageSexSuffix %in% c("05to09y", "10to14y")){
  csmf_HMM <- fn_capMalFrac(csmf_HMM, cases_malaria_05to19, frac_malaria_01to04)
  csmf_LMM <- fn_setMalFrac(csmf_LMM)
  csmfDraws_HMM <- lapply(csmfDraws_HMM, function(x){fn_capMalFrac(x, cases_malaria_05to19, frac_malaria_01to04) })
  csmfDraws_LMM <- lapply(csmfDraws_LMM, function(x){fn_setMalFrac(x) })
}

# Format predicted CSMFs
csmf <- fn_formatPrediction(csmf_HMM, csmf_LMM)
csmfDraws <- mapply(fn_formatPrediction, csmfDraws_HMM, csmfDraws_LMM, SIMPLIFY = FALSE)

# Save
write.csv(csmf, paste0("./gen/prediction/output/csmf_", ageSexSuffix, ".csv"), row.names = FALSE)
saveRDS(csmfDraws, paste0("./gen/uncertainty/input/csmfDraws_", ageSexSuffix, ".rds"))

# Unload MASS which was loaded for fn_pr2 and is masking dplyr::select()
detach("package:MASS")
