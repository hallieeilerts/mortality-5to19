################################################
# Data management
################################################

# Clear environment
rm(list = ls())

# Pull data from Data Warehouse
# change to pull all data in prepare session
#source("./src/data-management/pull-data.R",  local=new.env())

# Classification keys
source("./src/data-management/set-regions.R",  local=new.env())
source("./src/data-management/set-old-country-class.R",  local=new.env())
source("./src/data-management/set-country-class.R",  local=new.env())
source("./src/data-management/set-agesexgrp.R",  local=new.env())
source("./src/data-management/set-cod-reclass.R",  local=new.env())  # AgeSp
source("./src/data-management/set-codlist.R",  local=new.env())      # AgeSp

# Envelopes
source("./src/data-management/prep-envelopes.R", local = new.env())  # AgeSp
source("./src/data-management/prep-envelopes-regional.R", local = new.env()) # Happens here because need classification key formatted and single cause database has rows at country-level. Could switch to doing there at some point though. # AgeSp
# source("./src/data-management/prep-envelopes-draws.R") # For 15-19f or 15-19m, creates draws for males, females, and both sexes combined

# Prediction data
source("./src/data-management/prep-prediction-database.R", local = new.env())

# VR data
source("./src/data-management/prep-goodvr-new.R") # AgeSp
source("./src/data-management/prep-chinadsp.R")   # AgeSp

# Single-cause data
source("./src/data-management/prep-crisis.R", local = new.env())        # AgeSp
source("./src/data-management/prep-hiv.R", local = new.env())           # AgeSp
source("./src/data-management/prep-malaria-cases.R", local = new.env())
source("./src/data-management/prep-tb.R", local = new.env())            # AgeSp
if(ageSexSuffix == "05to09y"){source("./src/data-management/prep-measles.R", local = new.env())} # AgeSp

# Set fractions for capping malaria, and minimum fractions for squeezing
# Cases of malaria for identifying countries with any malaria
source("./src/data-management/set-frac-cap-malaria.R", local = new.env())
source("./src/data-management/set-frac-min-cd.R", local = new.env())
source("./src/data-management/set-frac-min-lri.R", local = new.env())



