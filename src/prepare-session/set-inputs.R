################################################################################
#' @description Manually sets age group and years being estimated.
#' @return Strings, booleans, integers, date defined below
################################################################################

## Choose age/sex group ## FORMERLY LABEL ageGroup
ageSexSuffix <- "05to09y"
#ageSexSuffix <- "10to14y"
#ageSexSuffix <- "15to19yF"
#ageSexSuffix <- "15to19yM"

## Set path to Data Warehouse for pulling latest data inputs
pathDataWarehouse <- "C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/CA-CODE/CA-CODE_DataWarehouse"

## Set years for update
Years <- 2000:2024

# Variables for sex
sexLabels <- c("Total", "Female", "Male")

# Vector with COD in correct order
codAll <- c("Measles", "Maternal", "HIV", "LRI",  "TB", "Diarrhoeal", "Malaria", "OtherCMPN",
            "Congenital", "Cardiovascular", "Digestive", "Neoplasms", "OtherNCD",
            "InterpVio","SelfHarm", "Drowning", "RTI", "OtherInj", "NatDis", "CollectVio")    

## Results date (for naming output files)
resDate <- format(Sys.Date(), format="%Y%m%d")
