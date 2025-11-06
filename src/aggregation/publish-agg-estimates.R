################################################
# Publish aggegated estimates
################################################

# Clear environment
rm(list = ls())

# Load section inputs and functions
source("./src/aggregation/aggregation_inputs.R")
source("./src/aggregation/aggregation_functions.R")

# Set results date to match date of unaggregated results
aggResDate <- "20251027"

# Publish point estimates from squeezing pipeline -------------------------------

# These are intermediate results while waiting on inputs for uncertainty pipeline.

# Load
csmfSqz_05to14 <- read.csv("./gen/aggregation/temp/csmfSqz_05to14y.csv")
csmfSqz_05to19 <- read.csv("./gen/aggregation/temp/csmfSqz_05to19y.csv")
csmfSqz_10to19 <- read.csv("./gen/aggregation/temp/csmfSqz_10to19y.csv")
csmfSqz_15to19 <- read.csv("./gen/aggregation/temp/csmfSqz_15to19y.csv")
csmfSqz_05to14REG <- read.csv("./gen/aggregation/temp/csmfSqz_05to14y_REG.csv")
csmfSqz_05to19REG <- read.csv("./gen/aggregation/temp/csmfSqz_05to19y_REG.csv")
csmfSqz_10to19REG <- read.csv("./gen/aggregation/temp/csmfSqz_10to19y_REG.csv")
csmfSqz_15to19REG <- read.csv("./gen/aggregation/temp/csmfSqz_15to19y_REG.csv")

# Perform rounding steps that occur in uncertainty pipeline
csmfSqz_ADJ_05to14 <- fn_adjustCSMFZeroDeaths(csmfSqz_05to14, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_ADJ_05to19 <- fn_adjustCSMFZeroDeaths(csmfSqz_05to19, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_ADJ_10to19 <- fn_adjustCSMFZeroDeaths(csmfSqz_10to19, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_ADJ_15to19 <- fn_adjustCSMFZeroDeaths(csmfSqz_15to19, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_ADJ_05to14REG <- fn_adjustCSMFZeroDeaths(csmfSqz_05to14REG, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_ADJ_05to19REG <- fn_adjustCSMFZeroDeaths(csmfSqz_05to19REG, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_ADJ_10to19REG <- fn_adjustCSMFZeroDeaths(csmfSqz_10to19REG, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_ADJ_15to19REG <- fn_adjustCSMFZeroDeaths(csmfSqz_15to19REG, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_FRMT_05to14 <- fn_roundCSMFsqz(csmfSqz_ADJ_05to14, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_FRMT_05to19 <- fn_roundCSMFsqz(csmfSqz_ADJ_05to19, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_FRMT_10to19 <- fn_roundCSMFsqz(csmfSqz_ADJ_10to19, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_FRMT_15to19 <- fn_roundCSMFsqz(csmfSqz_ADJ_15to19, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_FRMT_05to14REG <- fn_roundCSMFsqz(csmfSqz_ADJ_05to14REG, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_FRMT_05to19REG <- fn_roundCSMFsqz(csmfSqz_ADJ_05to19REG, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_FRMT_10to19REG <- fn_roundCSMFsqz(csmfSqz_ADJ_10to19REG, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_FRMT_15to19REG <- fn_roundCSMFsqz(csmfSqz_ADJ_15to19REG, AGGAGE = TRUE, CODALL = codAll)

# Format estimates
csmfSqz_PUB_05to14  <- fn_publishEstimates(DAT = csmfSqz_FRMT_05to14, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_PUB_05to19  <- fn_publishEstimates(DAT = csmfSqz_FRMT_05to19, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_PUB_10to19  <- fn_publishEstimates(DAT = csmfSqz_FRMT_10to19, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_PUB_15to19  <- fn_publishEstimates(DAT = csmfSqz_FRMT_15to19, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, AGGAGE = TRUE, CODALL = codAll)
csmfSqz_PUB_05to14REG  <- fn_publishEstimates(DAT = csmfSqz_FRMT_05to14REG, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, REGIONAL = TRUE, AGGAGE = TRUE, CODALL = codAll, AGEGROUP = "05to14y")
csmfSqz_PUB_05to19REG  <- fn_publishEstimates(DAT = csmfSqz_FRMT_05to19REG, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, REGIONAL = TRUE, AGGAGE = TRUE, CODALL = codAll, AGEGROUP = "05to19y")
csmfSqz_PUB_10to19REG  <- fn_publishEstimates(DAT = csmfSqz_FRMT_10to19REG, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, REGIONAL = TRUE, AGGAGE = TRUE, CODALL = codAll, AGEGROUP = "10to19y")
csmfSqz_PUB_15to19REG  <- fn_publishEstimates(DAT = csmfSqz_FRMT_15to19REG, KEY_REGION = key_region_u20, KEY_CTRYCLASS = key_ctryclass_u20, REGIONAL = TRUE, AGGAGE = TRUE, CODALL = codAll, AGEGROUP = "15to19y")

# # Save
write.csv(csmfSqz_PUB_05to14, paste("./gen/aggregation/output/PointEstimatesAggregated_National_05to14y_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_05to19, paste("./gen/aggregation/output/PointEstimatesAggregated_National_05to19y_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_10to19, paste("./gen/aggregation/output/PointEstimatesAggregated_National_10to19y_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_15to19, paste("./gen/aggregation/output/PointEstimatesAggregated_National_15to19y_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_05to14REG, paste("./gen/aggregation/output/PointEstimatesAggregated_Regional_05to14y_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_05to19REG, paste("./gen/aggregation/output/PointEstimatesAggregated_Regional_05to19y_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_10to19REG, paste("./gen/aggregation/output/PointEstimatesAggregated_Regional_10to19y_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_15to19REG, paste("./gen/aggregation/output/PointEstimatesAggregated_Regional_15to19y_", aggResDate, ".csv", sep=""), row.names = FALSE)

# Publish medians and uncertainty intervals from uncertainty pipeline -----------------------------

# Load
medInt_05to14 <- read.csv(paste("./gen/aggregation/temp/medInt_05to14_", aggResDate, ".csv", sep = ""))
medInt_05to19 <- read.csv(paste("./gen/aggregation/temp/medInt_05to19_", aggResDate, ".csv", sep = ""))
medInt_10to19 <- read.csv(paste("./gen/aggregation/temp/medInt_10to19_", aggResDate, ".csv", sep = ""))
medInt_15to19 <- read.csv(paste("./gen/aggregation/temp/medInt_15to19_", aggResDate, ".csv", sep = ""))
medInt_05to14REG <- read.csv(paste("./gen/aggregation/temp/medInt_05to14REG_", aggResDate, ".csv", sep = ""))
medInt_05to19REG <- read.csv(paste("./gen/aggregation/temp/medInt_05to19REG_", aggResDate, ".csv", sep = ""))
medInt_10to19REG <- read.csv(paste("./gen/aggregation/temp/medInt_10to19REG_", aggResDate, ".csv", sep = ""))
medInt_15to19REG <- read.csv(paste("./gen/aggregation/temp/medInt_15to19REG_", aggResDate, ".csv", sep = ""))

# Format estimates
med_PUB_05to14 <- fn_publishEstimates(medInt_05to14, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
med_PUB_05to19 <- fn_publishEstimates(medInt_05to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
med_PUB_10to19 <- fn_publishEstimates(medInt_10to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
med_PUB_15to19 <- fn_publishEstimates(medInt_15to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
med_PUB_05to14REG <- fn_publishEstimates(medInt_05to14REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
med_PUB_05to19REG <- fn_publishEstimates(medInt_05to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
med_PUB_10to19REG <- fn_publishEstimates(medInt_10to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
med_PUB_15to19REG <- fn_publishEstimates(medInt_15to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
medInt_PUB_05to14 <- fn_publishEstimates(medInt_05to14, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
medInt_PUB_05to19 <- fn_publishEstimates(medInt_05to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
medInt_PUB_10to19 <- fn_publishEstimates(medInt_10to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
medInt_PUB_15to19 <- fn_publishEstimates(medInt_15to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
medInt_PUB_05to14REG <- fn_publishEstimates(medInt_05to14REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
medInt_PUB_05to19REG <- fn_publishEstimates(medInt_05to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
medInt_PUB_10to19REG <- fn_publishEstimates(medInt_10to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
medInt_PUB_15to19REG <- fn_publishEstimates(medInt_15to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)

# Save
write.csv(med_PUB_05to14, paste("./gen/aggregation/output/MedianEstimates_National_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(med_PUB_05to19, paste("./gen/aggregation/output/MedianEstimates_National_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(med_PUB_10to19, paste("./gen/aggregation/output/MedianEstimates_National_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(med_PUB_15to19, paste("./gen/aggregation/output/MedianEstimates_National_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(med_PUB_05to14REG, paste("./gen/aggregation/output/MedianEstimates_Regional_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(med_PUB_05to19REG, paste("./gen/aggregation/output/MedianEstimates_Regional_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(med_PUB_10to19REG, paste("./gen/aggregation/output/MedianEstimates_Regional_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(med_PUB_15to19REG, paste("./gen/aggregation/output/MedianEstimates_Regional_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_PUB_05to14, paste("./gen/aggregation/output/UncertaintyMedians_National_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_PUB_05to19, paste("./gen/aggregation/output/UncertaintyMedians_National_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_PUB_10to19, paste("./gen/aggregation/output/UncertaintyMedians_National_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_PUB_15to19, paste("./gen/aggregation/output/UncertaintyMedians_National_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_PUB_05to14REG, paste("./gen/aggregation/output/UncertaintyMedians_Regional_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_PUB_05to19REG, paste("./gen/aggregation/output/UncertaintyMedians_Regional_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_PUB_10to19REG, paste("./gen/aggregation/output/UncertaintyMedians_Regional_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_PUB_15to19REG, paste("./gen/aggregation/output/UncertaintyMedians_Regional_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)

# Publish combination of point estimates and uncertainty intervals --------

# Load
pointInt_05to14 <- read.csv(paste("./gen/aggregation/temp/pointInt_05to14_", aggResDate, ".csv", sep = ""))
pointInt_05to19 <- read.csv(paste("./gen/aggregation/temp/pointInt_05to19_", aggResDate, ".csv", sep = ""))
pointInt_10to19 <- read.csv(paste("./gen/aggregation/temp/pointInt_10to19_", aggResDate, ".csv", sep = ""))
pointInt_15to19 <- read.csv(paste("./gen/aggregation/temp/pointInt_15to19_", aggResDate, ".csv", sep = ""))
pointInt_05to14REG <- read.csv(paste("./gen/aggregation/temp/pointInt_05to14REG_", aggResDate, ".csv", sep = ""))
pointInt_05to19REG <- read.csv(paste("./gen/aggregation/temp/pointInt_05to19REG_", aggResDate, ".csv", sep = ""))
pointInt_10to19REG <- read.csv(paste("./gen/aggregation/temp/pointInt_10to19REG_", aggResDate, ".csv", sep = ""))
pointInt_15to19REG <- read.csv(paste("./gen/aggregation/temp/pointInt_15to19REG_", aggResDate, ".csv", sep = ""))

# Format estimates
point_PUB_05to14 <- fn_publishEstimates(pointInt_05to14, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
point_PUB_05to19 <- fn_publishEstimates(pointInt_05to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
point_PUB_10to19 <- fn_publishEstimates(pointInt_10to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
point_PUB_15to19 <- fn_publishEstimates(pointInt_15to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
point_PUB_05to14REG <- fn_publishEstimates(pointInt_05to14REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
point_PUB_05to19REG <- fn_publishEstimates(pointInt_05to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
point_PUB_10to19REG <- fn_publishEstimates(pointInt_10to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
point_PUB_15to19REG <- fn_publishEstimates(pointInt_15to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
pointInt_PUB_05to14 <- fn_publishEstimates(pointInt_05to14, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_05to19 <- fn_publishEstimates(pointInt_05to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_10to19 <- fn_publishEstimates(pointInt_10to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_15to19 <- fn_publishEstimates(pointInt_15to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_05to14REG <- fn_publishEstimates(pointInt_05to14REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
pointInt_PUB_05to19REG <- fn_publishEstimates(pointInt_05to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
pointInt_PUB_10to19REG <- fn_publishEstimates(pointInt_10to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
pointInt_PUB_15to19REG <- fn_publishEstimates(pointInt_15to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)

# Save
write.csv(point_PUB_05to14, paste("./gen/aggregation/output/PointEstimates_National_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to19, paste("./gen/aggregation/output/PointEstimates_National_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_10to19, paste("./gen/aggregation/output/PointEstimates_National_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_15to19, paste("./gen/aggregation/output/PointEstimates_National_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to14REG, paste("./gen/aggregation/output/PointEstimates_Regional_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_10to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_15to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to14, paste("./gen/aggregation/output/Uncertainty_National_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to19, paste("./gen/aggregation/output/Uncertainty_National_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_10to19, paste("./gen/aggregation/output/Uncertainty_National_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_15to19, paste("./gen/aggregation/output/Uncertainty_National_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to14REG, paste("./gen/aggregation/output/Uncertainty_Regional_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to19REG, paste("./gen/aggregation/output/Uncertainty_Regional_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_10to19REG, paste("./gen/aggregation/output/Uncertainty_Regional_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_15to19REG, paste("./gen/aggregation/output/Uncertainty_Regional_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)


