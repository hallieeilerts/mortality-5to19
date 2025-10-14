################################################################################
#' @description Create variables related to the age-sex group under analysis.
#' @return Strings, booleans defined below
################################################################################
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
################################################################################

if(!exists("ageSexSuffix")){
  stop("ageSexSuffix does not exist. Choose ageSexSuffix in set-inputs.R")
}
if(!exists("Years")){
  stop("Years does not exist. Choose Years in set-inputs.R")
}

# Indicator that session variables have been created
sessionVars <- TRUE

# Data frame with information on modeled age/sex groups
#ageSexGroups <- read.csv("./data/classification-keys/key_agesexgroups.csv")

# Age group labels
ageLow <- as.numeric(sub("to.*", "", ageSexSuffix))
ageUp <- as.numeric(gsub("[[:alpha:]]", "", sub(".*to", "", ageSexSuffix)))
ageUp  <- ifelse(ageSexSuffix == "00to28d", 1/12, ageUp)
ageLow <- ifelse(ageSexSuffix == "01to59m", 28/365.25, ageLow)
ageUp  <- ifelse(ageSexSuffix == "01to59m", 59/12, ageUp)

# Sex split
sexSplit <- ifelse(ageLow == 15, TRUE, FALSE)

# Create variable for sex
if(sexSplit){
  if (ageSexSuffix == "15to19yF") sexLabel <- "Female"
  if (ageSexSuffix == "15to19yM") sexLabel <- "Male"
}else{
  sexLabel <- "Total"
}
sexLabels <- c("Total", "Female", "Male") 

# Create suffix for sex
if(sexSplit){
  if (ageSexSuffix == "15to19yF") sexSuffix <- "f"
  if (ageSexSuffix == "15to19yM") sexSuffix <- "m"
}else{
  sexSuffix <- "mf"
}

# Create variable for respiratory TB
respTB <- ifelse(ageSexSuffix %in% c("05to09y", "10to14y"), TRUE, FALSE)

# Variables to uniquely identify records
#idVars <- c("ISO3", "Year", "Sex")
idVars <- c("iso3", "year")

# Vector with COD in correct order
codAll <- c("Measles", "Maternal", "HIV", "LRI",  "TB", "Diarrhoeal", "Malaria", "OtherCMPN",
            "Congenital", "Cardiovascular", "Digestive", "Neoplasms", "OtherNCD",
            "InterpVio","SelfHarm", "Drowning", "RTI", "OtherInj", "NatDis", "CollectVio")    

# Save names of all session variables
sessionVarsList <- ls()
