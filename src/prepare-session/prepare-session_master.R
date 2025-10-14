################################################
# Prepare session
################################################

# Set ageGroup and Years for estimation
source("./src/prepare-session/set-inputs.R")

# Create session variables for inputs
source("./src/prepare-session/create-session-variables.R")

# Load required functions
source("./src/prepare-session/prepare-session_functions.R")

# Pull data
source("./src/prepare-session/pull-data.R")