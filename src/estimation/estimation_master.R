################################################
# Estimation
################################################

# Clear environment
rm(list = ls())

# Load functions
source("./src/estimation/estimation_inputs.R")

# Create model input (studies, deaths, vdt, vxf, nchai, niter, nburn)
mod_input_HMM <- fn_createModInput(ageSexSuffix, "HMM", mod_dat_HMM, dat_covar, dat_cod, dat_hp)
mod_input_LMM <- fn_createModInput(ageSexSuffix, "LMM", mod_dat_LMM, dat_covar, dat_cod, dat_hp)

# Estimate HMM
st.data <- mod_input_HMM$st_data
st.input <- mod_input_HMM$st_input
jobRunScript("./src/estimation/runStan.R", importEnv=TRUE, exportEnv="")

# Estimate LMM
st.data <- mod_input_LMM$st_data
st.input <- mod_input_LMM$st_input
jobRunScript("./src/estimation/runStan.R", importEnv=TRUE, exportEnv="")
