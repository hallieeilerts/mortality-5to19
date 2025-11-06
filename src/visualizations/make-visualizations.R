################################################
# Visualizations
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/visualizations/visualizations_inputs.R")
source("./src/visualizations/visualizations_functions.R")

# Audit -------------------------------------------------------------------

# Compare between my and Pancho's estimates for this round
# I have restructured his code, but these should match almost exactly.

# Reshape Pancho's regional results file
point_PanchoResultsFRMT <- fn_reshapePanchoPointDP(point_PanchoResults, key_ctryclassOld, codAll, ageSexSuffix)
point_PanchoResultsFRMT_REG <- fn_reshapePanchoRegAndAgg(point_PanchoResults_REG, codAll)

## Plots showing cumulative updates to estimates

# plot <- fn_compareCSMF(point_PanchoResultsFRMT, new_covarval, new_covarset, new_sc, new_hp, SAMPLE = NULL, 
#                        LEVEL1 = "Simple update 2021", 
#                        LEVEL2 = "New covar values",
#                        LEVEL3 = "New covar set",
#                        LEVEL4 = "New single causes",
#                        LEVEL5 = "New HP",
#                        CTRYGRP = "HMM")
# ggsave(paste("./gen/visualizations/audit/buildup_HMM_", ageSexSuffix,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# plot <- fn_compareCSMF(point_PanchoResultsFRMT, new_covarval, new_covarset, new_stan, 
#                        SAMPLE = NULL, 
#                        LEVEL1 = "Simple update 2021", 
#                        LEVEL2 = "New covar values",
#                        LEVEL3 = "New covar set",
#                        LEVEL4 = "Stan",
#                        CTRYGRP = "HMM")
# ggsave(paste("./gen/visualizations/audit/buildup2_HMM_", ageSexSuffix,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# plot <- fn_compareCSMF(point_PanchoResultsFRMT, new_covarset, new_stan, new_sc, new_hp,
#                        SAMPLE = NULL, 
#                        LEVEL1 = "Simple update 2021", 
#                        LEVEL2 = "New covar set",
#                        LEVEL3 = "New stan",
#                        LEVEL4 = "New single causes",
#                        LEVEL5 = "New HP",
#                        CTRYGRP = "HMM")
# ggsave(paste("./gen/visualizations/audit/buildup3_HMM_", ageSexSuffix,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareCSMF(point_PanchoResultsFRMT, new_sm, new_vr, SAMPLE = NULL,
                       LEVEL1 = "Simple update 2021",
                       LEVEL2 = "Preliminary estimates 2023",
                       LEVEL3 = "Extrap VR to 2024",
                       CTRYGRP = NULL)
ggsave(paste("./gen/visualizations/audit/csmf_national_", ageSexSuffix,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")

lam12 <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922.csv", sep=""))
lam15 <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922-lam15.csv", sep=""))
lam18 <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922-lam18.csv", sep=""))
lam20 <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922-lam20.csv", sep=""))
lam400 <- read.csv(paste("./gen/results/temp/SmCov-PointEstimates_National_", ageSexSuffix,"_20250922-lam400.csv", sep=""))
plot <- fn_compareCSMF(point_PanchoResultsFRMT, lam12, lam15, lam20, lam400, SAMPLE = NULL,
                       LEVEL1 = "Simple update 2021",
                       LEVEL2 = "Lambda 12", LEVEL3 = "Lambda 15", LEVEL4 = "Lambda 20",
                       LEVEL5 = "Lambda 400",
                       CTRYGRP = "HMM")
ggsave(paste("./gen/visualizations/audit/csmf_national_", ageSexSuffix,"_", resDate, "-lamCompare.pdf", sep=""), plot, height = 10, width = 8, units = "in")


# # Uncertainty intervals
# plot <- fn_compareUI(pointInt, pointInt_PanchoResults, VARIABLE = "Fraction", CODALL = codAll, SAMPLE = NULL)
# ggsave(paste("./gen/visualizations/audit/ui_comparison_national_", ageSexSuffix,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# plot <- fn_compareUI(pointInt_REG, point_PanchoResultsFRMT_REG, VARIABLE = "Fraction", CODALL = codAll, REGIONAL = TRUE)
# ggsave(paste("./gen/visualizations/audit/ui_comparison_regional_", ageSexSuffix,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# plot <- fn_compareUIfixY(pointInt_REG, point_PanchoResultsFRMT_REG, VARIABLE = "Fraction", CODALL = codAll, REGIONAL = TRUE)
# ggsave(paste("./gen/visualizations/audit/ui_comparison_regional_", ageSexSuffix,"_", resDate, "fixedY.pdf", sep=""), plot, height = 10, width = 8, units = "in")

