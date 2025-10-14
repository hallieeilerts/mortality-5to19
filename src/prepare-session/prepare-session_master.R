################################################
# Prepare session
################################################

# Set age group, years, path to data warehouse
source("./src/prepare-session/set-inputs.R")

# Load required functions
source("./src/prepare-session/prepare-session_functions.R")

# Pull data
source("./src/prepare-session/pull-data.R")