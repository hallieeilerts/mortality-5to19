################################################################################
#' @description Manually sets age group and years being estimated.
#' @return Strings, booleans, integers, date defined below
################################################################################

## Type of update
simpleUpdate <- FALSE

## Choose age/sex group ## FORMERLY LABEL ageGroup
#ageSexSuffix <- "05to09y"
#ageSexSuffix <- "10to14y"
#ageSexSuffix <- "15to19yF"
ageSexSuffix <- "15to19yM"

## Set path to Data Warehouse for pulling latest data inputs
pathDataWarehouse <- "C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/CA-CODE/CA-CODE_DataWarehouse"

## Set years for update
Years <- 2000:2024

## Results date (for naming output files)
resDate <- format(Sys.Date(), format="%Y%m%d")
