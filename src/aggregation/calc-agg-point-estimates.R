################################################
# Aggregated Point Estimates
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/aggregation/aggregation_inputs.R")
source("./src/aggregation/aggregation_functions.R")

# Envelopes
env_u20 <- read.csv("./gen/data-management/output/env_u20.csv")
# Regional envelopes provided by IGME
env_10to14REG <- read.csv(paste("./gen/data-management/output/env_10to14y_REG.csv", sep=""))
env_15to19REG <- read.csv(paste("./gen/data-management/output/env_15to19y_REG.csv", sep=""))

# CSMFs that have been processed in squeezing pipeline for all age groups
csmfSqz_05to09 <- read.csv(paste("./gen/squeezing/output/csmfSqz_05to09y.csv", sep = ""))
csmfSqz_10to14 <- read.csv(paste("./gen/squeezing/output/csmfSqz_10to14y.csv", sep = ""))
csmfSqz_15to19f <- read.csv(paste("./gen/squeezing/output/csmfSqz_15to19yF.csv", sep = ""))
csmfSqz_15to19m <- read.csv(paste("./gen/squeezing/output/csmfSqz_15to19yM.csv", sep = ""))

# Calculate aggregate point estimates -----------------------------------------------

csmfSqz_05to14 <- fn_calcAggAges(AGELB = 5, AGEUB = 14, CODALL = codAll, CSMF_5TO9 = csmfSqz_05to09,  CSMF_10TO14 = csmfSqz_10to14)
csmfSqz_05to19 <- fn_calcAggAges(AGELB = 5, AGEUB = 19, CODALL = codAll, CSMF_5TO9 = csmfSqz_05to09,  CSMF_10TO14 = csmfSqz_10to14, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)
csmfSqz_10to19 <- fn_calcAggAges(AGELB = 10, AGEUB = 19, CODALL = codAll, CSMF_10TO14 = csmfSqz_10to14, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)
csmfSqz_15to19 <- fn_calcAggAges(AGELB = 15, AGEUB = 19, CODALL = codAll, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)

# Calculate regional CSMFs  for aggregate age groups
csmfSqz_05to14REG <- fn_calcRegion(CSMF = csmfSqz_05to14, ENV_REGION = NULL, KEY_REGION = key_region_u20, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_05to19REG <- fn_calcRegion(CSMF = csmfSqz_05to19, ENV_REGION = NULL, KEY_REGION = key_region_u20, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_10to19REG <- fn_calcRegion(CSMF = csmfSqz_10to19, ENV_REGION = env_10to14REG, KEY_REGION = key_region_u20, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_15to19REG <- fn_calcRegion(CSMF = csmfSqz_15to19, ENV_REGION = env_15to19REG, KEY_REGION = key_region_u20, AGGAGE = TRUE, CODALL = codAll)

# Save point estimates
write.csv(csmfSqz_05to14, paste("./gen/aggregation/temp/csmfSqz_05to14y.csv", sep = ""), row.names = FALSE)
write.csv(csmfSqz_05to19, paste("./gen/aggregation/temp/csmfSqz_05to19y.csv", sep = ""), row.names = FALSE)
write.csv(csmfSqz_10to19, paste("./gen/aggregation/temp/csmfSqz_10to19y.csv", sep = ""), row.names = FALSE)
write.csv(csmfSqz_15to19, paste("./gen/aggregation/temp/csmfSqz_15to19y.csv", sep = ""), row.names = FALSE)
write.csv(csmfSqz_05to14REG, paste("./gen/aggregation/temp/csmfSqz_05to14y_REG.csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_05to19REG, paste("./gen/aggregation/temp/csmfSqz_05to19y_REG.csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_10to19REG, paste("./gen/aggregation/temp/csmfSqz_10to19y_REG.csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_15to19REG, paste("./gen/aggregation/temp/csmfSqz_15to19y_REG.csv", sep=""), row.names = FALSE)

