################################################
# Data management
################################################

# Clear environment
rm(list = ls())

# Classification keys
source("./src/data-management/set-regions.R", local=new.env())
source("./src/data-management/set-old-country-class.R", local=new.env())
source("./src/data-management/set-country-class.R", local=new.env())
source("./src/data-management/set-agesexgrp.R", local=new.env())
source("./src/data-management/set-cod-reclass.R", local=new.env())
source("./src/data-management/set-codlist.R", local=new.env())

# Envelopes
source("./src/data-management/prep-envelopes.R", local = new.env())
source("./src/data-management/prep-envelopes-regional.R", local = new.env())

# Prediction data
source("./src/data-management/prep-prediction-database.R", local = new.env())

# VR data
source("./src/data-management/prep-goodvr.R")
source("./src/data-management/prep-chinadsp.R")

# Single-cause data (squeezing inputs)
source("./src/data-management/prep-crisis.R", local = new.env())
source("./src/data-management/prep-hiv.R", local = new.env())
source("./src/data-management/prep-malaria-cases.R", local = new.env())
source("./src/data-management/prep-tb.R", local = new.env())
if(ageSexSuffix == "05to09y"){source("./src/data-management/prep-measles.R", local = new.env())}

# Set fractions for capping malaria (prediction input)
source("./src/data-management/set-frac-cap-malaria.R", local = new.env())

# Set minimum fractions (squeezing inputs)
source("./src/data-management/set-frac-min-cd.R", local = new.env())
source("./src/data-management/set-frac-min-lri.R", local = new.env())

# IGME draws
source("./src/data-management/envdraws-load.R", local=new.env())
source("./src/data-management/envdraws-adjust-15to19.R", local=new.env())
source("./src/data-management/envdraws-format.R", local=new.env())
source("./src/data-management/envdraws-15to19-sexcombined.R", local=new.env())


