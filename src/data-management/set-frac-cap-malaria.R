################################################################################
#' @description Extract malaria fractions for 1-59m
#' @return Data frame with c("ISO3", "Year", "csmf_malaria_5to19")
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyr)
require(dplyr)
#' Inputs
source("./src/prepare-session/set-inputs.R")
csmf_01to04 <- read.csv("./data/previous-results/CA-CODE-2024-1to59m-National.csv")
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

dat <- subset(csmf_01to04, Indicator == "Fraction" & Cause.of.death == "Malaria")

# Rename variables
names(dat)[names(dat) == "REF_AREA"] <- "iso3"
names(dat)[names(dat) == "TIME_PERIOD"] <- "year"
names(dat)[names(dat) == "OBS_VALUE"] <- "csmf_malaria_01to04"
dat <- dat[,c("iso3", "year", "csmf_malaria_01to04")]

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$iso3) %in% dat$iso3)) > 0){
  warning("Not all countries included in data input.")
  write.table(sort(unique(key_ctryclass_u20$WHOname)[!(unique(key_ctryclass_u20$iso3) %in% dat$iso3)]), 
              "./gen/data-management/audit/missing_mal-frac-cap.txt")
}

# Fill in zeros for missing country-years, if necessary

# Create data frame for countries/years of interest
# For malaria fraction cap data, HMM countries
df_ctryyears <- data.frame(iso3 = rep(key_ctryclass_u20$iso3, each = length(Years)),
                           year = rep(Years))

# Merge onto COD data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = c("iso3", "year"), all = TRUE)

# Fill missing values -----------------------------------------------------

# Recode missing csmf 0
dat$csmf_malaria_01to04[which(is.na(dat$csmf_malaria_01to04) & dat$year <= 2021)] <- 0

# Extrapolate from 2021 to 2023
extend <- dat %>% 
  arrange(iso3, year) %>%
  filter(year >= 2021) %>% 
  group_by(iso3) %>%
  fill(csmf_malaria_01to04, .direction = "down") %>% 
  filter(year > 2021)

# Rbind extrapolated data
dat <- dat %>% filter(year <= 2021) %>%
  rbind(., extend)

# Tidy up
dat <- dat[, c("iso3", "year", "csmf_malaria_01to04")]
dat <- dat[order(dat$iso3, dat$year),]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/prediction/input/frac_malaria_01to04.csv", sep=""), row.names = FALSE)
