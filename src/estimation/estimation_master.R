################################################
# Estimation
################################################

# Clear environment
rm(list = ls())

# Load functions
source("./src/estimation/estimation_inputs.R")
source("./src/estimation/estimation_functions.R")

# Assess covariate correlation
#source("./src/estimation/check-covar-correlation.R", local = new.env())

# Load HMM and LMM model data
mod_dat_HMM <- fn_loadModData(ageSexSuffix, "HMM")
mod_dat_LMM <- fn_loadModData(ageSexSuffix, "LMM")

# # use old or alternative hyperparam values
# dat_hp$lamb[dat_hp$AgeSexSuffix == "05to09y" & dat_hp$Model == "HMM"] <- 100
# dat_hp$lamb[dat_hp$AgeSexSuffix == "10to14y" & dat_hp$Model == "HMM"] <- 50
# dat_hp$lamb[dat_hp$AgeSexSuffix == "15to19yF" & dat_hp$Model == "HMM"] <- 50
dat_hp$lamb[dat_hp$AgeSexSuffix == "15to19yM" & dat_hp$Model == "HMM"] <- 400

# Create model input (studies, deaths, vdt, vxf, nchai, niter, nburn)
mod_input_HMM <- fn_createModInput(ageSexSuffix, "HMM", mod_dat_HMM, dat_covar, dat_cod, dat_hp)
mod_input_LMM <- fn_createModInput(ageSexSuffix, "LMM", mod_dat_LMM, dat_covar, dat_cod, dat_hp)

# Estimate HMM
st.data <- mod_input_HMM$st_data
st.input <- mod_input_HMM$st_input
#st.name <- paste("Stan_HMM", ageSexSuffix, st.data$rsdlim, st.data$lambda, sep= "_")
jobRunScript("./src/estimation/runStan.R", importEnv=TRUE, exportEnv="")

# Estimate LMM - need to add hyperparameter values to dat_hp
st.data <- mod_input_LMM$st_data
st.input <- mod_input_LMM$st_input
#st.name <- paste("Stan_LMM", ageSexSuffix, st.data$rsdlim, st.data$lambda, sep= "_")
jobRunScript("./src/estimation/runStan.R", importEnv=TRUE, exportEnv="")
