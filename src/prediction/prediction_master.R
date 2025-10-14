################################################
# Prediction
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/prediction/prediction_inputs.R")
source("./src/prediction/prediction_functions.R")

# # old hyperparameter values
# dat_hp$lamb[dat_hp$AgeSexSuffix == "05to09y" & dat_hp$Model == "HMM"] <- 75
# dat_hp$lamb[dat_hp$AgeSexSuffix == "10to14y" & dat_hp$Model == "HMM"] <- 50
# dat_hp$lamb[dat_hp$AgeSexSuffix == "15to19yF" & dat_hp$Model == "HMM"] <- 50
dat_hp$lamb[dat_hp$AgeSexSuffix == "15to19yM" & dat_hp$Model == "HMM"] <- 400

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

# Set malaria fractions
if(ageSexSuffix %in% c("05to09y", "10to14y")){
  csmf_HMM <- fn_capMalFrac(csmf_HMM, cases_malaria_05to19, frac_malaria_01to04)
  csmf_LMM <- fn_setMalFrac(csmf_LMM)
}

# Format predicted CSMFs, save
csmf <- fn_formatPrediction(csmf_HMM, csmf_LMM)
write.csv(csmf, paste("./gen/prediction/output/csmf_", ageSexSuffix, ".csv",sep=""), row.names = FALSE)

# Unload MASS which was loaded for fn_pr2 and is masking dplyr::select()
#detach("package:MASS", unload=TRUE)
detach("package:ggforce")
detach("package:MASS")






# # Compare india RE to state averaged RE
# # run first with India state RE averaging
# dat1 <- subset(mod_predSum_HMM$Pred, type == "random") %>%
#   filter(type == "random" & iso3 == "IND") %>%
#   select(iso3, year, cause, p_50) %>%
#   mutate(source = "state RE avg")
# # Comment India state RE averaging out in fn_pr2 and run again
# dat2 <- subset(mod_predSum_HMM$Pred, type == "random") %>%
#   filter(type == "random" & iso3 == "IND") %>%
#   select(iso3, year, cause, p_50) %>%
#   mutate(source = "nat RE")
# dat3 <- rbind(dat1, dat2)
# dat3 %>%
#   ggplot(aes(x = year, y = p_50, col = source)) +
#   geom_point() + geom_line() +
#   facet_wrap(~cause)

# check_rhat <- summary(mod_fit_HMM$st.output)$summary
# check_rhat %>%
#   as.data.frame() %>%
#   rownames_to_column(var = "param") %>%
#   mutate(param_type = substr(param, 1, 2)) %>%
#   group_by(param_type) %>%
#   summarise(mean(Rhat))
