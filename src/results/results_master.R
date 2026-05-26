################################################
# Results
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/results/results_inputs.R")
source("./src/results/results_functions.R")

# Publish estimates from squeezing pipeline -------------------------------

# These are intermediate results while waiting on inputs for uncertainty pipeline.

# Perform rounding steps that occur in uncertainty pipeline
csmfPoint_ADJ <- fn_adjustCSMFZeroDeaths(csmfPoint, key_codlist)
csmfPoint_FRMT <- fn_roundCSMFpoint(csmfPoint_ADJ, key_codlist)
csmfPoint_ADJ_REG <- fn_adjustCSMFZeroDeaths(csmfPoint_REG, key_codlist)
csmfPoint_FRMT_REG <- fn_roundCSMFpoint(csmfPoint_ADJ_REG, key_codlist)

# Format estimates
csmfPoint_PUB <- fn_publishEstimates(DAT = csmfPoint_FRMT, KEY_CODLIST = key_codlist, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20,  KEY_AGESEXGRP = key_agesexgrp)
csmfPoint_PUB_REG  <- fn_publishEstimates(DAT = csmfPoint_FRMT_REG, KEY_CODLIST = key_codlist, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, KEY_AGESEXGRP = key_agesexgrp, REGIONAL = TRUE)

# Save
write.csv(csmfPoint_PUB, paste("./gen/results/output/PointEstimates_National_", ageSexSuffix, "_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfPoint_PUB_REG, paste("./gen/results/output/PointEstimates_Regional_", ageSexSuffix, "_", resDate, ".csv", sep=""), row.names = FALSE)

# Publish estimates from uncertainty pipeline -----------------------------

# These are the final results.

# Format estimates
point_PUB        <- fn_publishEstimates(pointInt, KEY_CODLIST = key_codlist, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, KEY_AGESEXGRP = key_agesexgrp, UNCERTAINTY = FALSE)
pointInt_PUB     <- fn_publishEstimates(pointInt,KEY_CODLIST = key_codlist, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, KEY_AGESEXGRP = key_agesexgrp, UNCERTAINTY = TRUE)
point_PUB_REG    <- fn_publishEstimates(pointInt_REG, KEY_CODLIST = key_codlist, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, KEY_AGESEXGRP = key_agesexgrp, UNCERTAINTY = FALSE, REGIONAL = TRUE)
pointInt_PUB_REG <- fn_publishEstimates(pointInt_REG, KEY_CODLIST = key_codlist, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, KEY_AGESEXGRP = key_agesexgrp, UNCERTAINTY = TRUE, REGIONAL = TRUE)

# Save
write.csv(point_PUB, paste("./gen/results/output/PointEstimates_National_", ageSexSuffix, "_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB, paste("./gen/results/output/Uncertainty_National_", ageSexSuffix, "_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_REG, paste("./gen/results/output/PointEstimates_Regional_", ageSexSuffix, "_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_REG, paste("./gen/results/output/Uncertainty_Regional_", ageSexSuffix, "_", resDate, ".csv", sep=""), row.names = FALSE)


