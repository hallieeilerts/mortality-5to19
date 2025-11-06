################################################################################
#' @description Loads all libraries and inputs required for Uncertainty
#' @return Inputs loaded below
################################################################################
#' Libraries
#require(plyr)  # ldply(), dlply
library(dplyr)
library(tidyr)
#library(data.table) # melt(), dcast()
library(ggplot2)
library(gridExtra)
#' Inputs
source("./src/prepare-session/set-inputs.R")
# Classification keys
codAll <- c("Measles", "Maternal", "HIV", "LRI",  "TB", "Diarrhoeal", "Malaria", "OtherCMPN",
            "Congenital", "Cardiovascular", "Digestive", "Neoplasms", "OtherNCD",
            "InterpVio","SelfHarm", "Drowning", "RTI", "OtherInj", "NatDis", "CollectVio")   
key_region_u20     <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_ctryclass_u20  <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################