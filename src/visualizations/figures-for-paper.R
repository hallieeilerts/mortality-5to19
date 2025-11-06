################################################################################
#' @description Generate preliminary figures for BMJ paper
#' @return Figures
################################################################################
# Clear environment
rm(list = ls())
#' Libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(patchwork)
library(scales)
#' Inputs
source("./src/prepare-session/set-inputs.R")
# Classification keys
key_ctryclassOld <- read.csv("./gen/data-management/output/key_ctryclassOld_u20.csv")
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
# National-level results - preliminary
nat05to09 <- read.csv(paste("./gen/results/temp/PointEstimates_National_05to09y_20250926.csv", sep=""))
nat05to09$AgeSexSuffix <- "05to09y"
nat10to14 <- read.csv(paste("./gen/results/temp/PointEstimates_National_10to14y_20250926.csv", sep=""))
nat10to14$AgeSexSuffix <- "10to14y"
nat15to19f <- read.csv(paste("./gen/results/temp/PointEstimates_National_15to19yF_20250926.csv", sep=""))
nat15to19f$AgeSexSuffix <- "15to19yF"
nat15to19m <- read.csv(paste("./gen/results/temp/PointEstimates_National_15to19yM_20250926.csv", sep=""))
nat15to19m$AgeSexSuffix <- "15to19yM"
allNat <- bind_rows(nat05to09, nat10to14, nat15to19f, nat15to19m)
# Region-level results - preliminary
reg05to09 <- read.csv(paste("./gen/results/output/PointEstimates_Regional_05to09y_20251027.csv", sep=""))
reg05to09$AgeSexSuffix <- "05to09y"
reg10to14 <- read.csv(paste("./gen/results/output/PointEstimates_Regional_10to14y_20251027.csv", sep=""))
reg10to14$AgeSexSuffix <- "10to14y"
reg15to19f <- read.csv(paste("./gen/results/output/PointEstimates_Regional_15to19yF_20251022.csv", sep=""))
reg15to19f$AgeSexSuffix <- "15to19yF"
reg15to19m <- read.csv(paste("./gen/results/output/PointEstimates_Regional_15to19yM_20251022.csv", sep=""))
reg15to19m$AgeSexSuffix <- "15to19yM"
allReg <- bind_rows(reg05to09, reg10to14, reg15to19f, reg15to19m)
# Aggregate age groups - preliminary
aggNat5to19 <- read.csv(paste("./gen/aggregation/output/PointEstimatesAggregated_National_05to19y_20251027.csv", sep=""))
aggReg5to19 <- read.csv(paste("./gen/aggregation/output/PointEstimatesAggregated_Regional_05to19y_20251027.csv", sep=""))
################################################################################


# Data prep ---------------------------------------------------------------

# cod vector
v_cod <- names(aggReg5to19)[!(names(aggReg5to19) %in% c("Region", "Year", "AgeGroup", "Sex", "Deaths", "Rate"))]
v_cod[1] <- "Maternal"
v_cod[2] <- "Measles"
v_codn <- paste0(seq(1, length(v_cod), 1), ". ", v_cod)

# region order
v_reg <- c("Eastern and Southern Africa", "West and central Africa", "Middle East and North Africa",
           "South Asia", "East Asia and Pacific", 
           "Latin America and Caribbean", "North America" ,
           "Eastern Europe and central Asia","Western Europe",
           "World")

# region order by n 5-19 deaths in 2024
v_reg_dths <- aggReg5to19 %>%
  filter(Year == 2024) %>%
  arrange(-Deaths) %>%
  select(Region) %>%
  pull()


# cod groups
df_codgrp <- data.frame(COD = v_cod,
                        CODgrp = c(rep("communicable", 8), rep("ncd", 5), rep("injury", 7)))


# country classifications used in model prediction
key_ctryclass %>%
  group_by(Group2010) %>%
  summarise(n())

# function to add dummy legend
dummy_guide <- function(
    labels = NULL,  
    ..., 
    title = NULL, 
    key   = draw_key_point,
    guide_args = list()
) {
  # Capture arguments
  aesthetics <- list(...)
  n <- max(lengths(aesthetics), 0)
  labels <- labels %||%  seq_len(n)
  
  # Overrule the alpha = 0 that we use to hide the points
  aesthetics$alpha <- aesthetics$alpha %||% rep(1, n)
  
  # Construct guide
  guide_args$override.aes <- guide_args$override.aes %||% aesthetics
  guide <- do.call(guide_legend, guide_args)
  
  # Allow dummy aesthetic
  update_geom_defaults("point", list(dummy = "x"))
  
  dummy_geom <- geom_point(
    data = data.frame(x = rep(Inf, n), y = rep(Inf, n), 
                      dummy = factor(labels)),
    aes(x, y, dummy = dummy), alpha = 0, key_glyph = key
  )
  dummy_scale <- discrete_scale(
    "dummy", "dummy_scale", palette = scales::identity_pal(), name = title,
    guide = guide
  )
  list(dummy_geom, dummy_scale)
}


# Numbers for Methods -----------------------------------------------------

# Number of HMM, LMM, VR countries
length(unique(nat05to09$ISO3)) # 195
nat05to09 %>%
  select(ISO3, Model) %>%
  distinct() %>%
  group_by(Model) %>%
  summarise(n = n())

# Numbers for beginning of Results section -------------------------------------------------------

# Global deaths 5-19y in 2024
aggReg5to19 %>%
  filter(Region == "World" & Year == 2024) %>%
  select(Deaths)

# Global deaths 5-19y due to CMPN, NCDs, injuries in 2024
aggReg5to19 %>%
  filter(Region == "World" & Year == 2024) %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  left_join(df_codgrp, by = "COD") %>%
  mutate(deaths = CSMF * Deaths) %>%
  group_by(CODgrp) %>%
  summarise(Deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(total = sum(Deaths),
         per = Deaths/total)

# Global leading causes of death 5-19y in 2024
aggReg5to19 %>%
  filter(Region == "World" & Year == 2024) %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  arrange(-CSMF) %>%
  mutate(per = Deaths*CSMF)

# top 5 causes in each age group in 2024
allReg %>%
  filter(Region == "World" & Year == 2024) %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  filter(!(COD %in% c("OtherCMPN", "OtherNCD", "OtherInj"))) %>%
  mutate(CSD = Deaths*CSMF) %>%
  arrange(AgeSexSuffix, -CSD) %>%
  group_by(AgeSexSuffix) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <=5) %>%
  select(AgeSexSuffix, COD, CSD)


# All-cause deaths 5-19y in vr, china, hmm, lmm countries in 2024
aggNat5to19 %>%
  filter(Year == 2024) %>%
  left_join(key_ctryclass %>% select(iso3, Group2010), by = c("ISO3" = "iso3")) %>%
  group_by(Group2010) %>%
  summarise(Deaths = sum(Deaths)) %>%
  ungroup() %>%
  mutate(total = sum(Deaths),
         per = Deaths/total)

# Number of vr, china, hmm, lmm countries by region
key_ctryclass %>% 
  select(iso3, Group2010) %>%
  left_join(aggNat5to19 %>% select(ISO3, Region) %>% distinct(), by = c("iso3" = "ISO3")) %>%
  group_by(Group2010, Region) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = round(n/total*100,2)) %>%
  arrange(Group2010, -n)

# 5-19y deaths in vr, china, hmm, lmm countries by region
aggNat5to19 %>% 
  filter(Year == 2024) %>%
  select(ISO3, Region, Deaths) %>%
  left_join(key_ctryclass %>% select(iso3, Group2010), by = c("ISO3" = "iso3")) %>%
  group_by(Group2010, Region) %>%
  summarise(Deaths = sum(Deaths)) %>%
  group_by(Group2010) %>%
  mutate(total = sum(Deaths),
         per = round(Deaths/total*100,2))

# 5-19y deaths by region in 2024
aggReg5to19 %>%
  filter(Region != "World" & Year == 2024) %>%
  select(Region, Deaths) %>%
  mutate(total = sum(Deaths),
         per = round(Deaths/total*100,2))

# Figure 1: global pie chart 5-19y 2024 ----------------------------------------------

p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  filter(Region == "World", Year == 2024) %>%
  mutate(
    COD = factor(COD, levels = v_cod),
    CSMF_pct   = CSMF * 100,
    CSMFround  = round(CSMF_pct, 0),
    label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "")
  ) %>%
  ggplot(aes(x = "", y = CSMF_pct, fill = COD)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(
    aes(label = label_text),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "bottom", legend.title = element_blank()) 
labs(title = "Global CSMFs 5-19y, 2024", fill = "COD")
ggsave(paste("./gen/visualizations/output/csmf_world_pie_05to19_2024.png", sep=""), p, dpi = 500, height = 6, width = 6, units = "in")

# Figure 2: cause of death distribution by region, 5-19y, 2024 --------------

p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod),
         Region = factor(Region, levels = v_reg_dths)) %>%
  filter(Region != "World") %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  pivot_longer(
    cols = c(Deaths, CSMF),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = Region, y = Value, fill = COD)) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  facet_wrap(~Variable, scales = "free_x") +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "", x= "", y = "")
ggsave(paste("./gen/visualizations/output/csmf_deaths_reg_05to19_2024.png", sep=""), p, dpi = 500, height = 6, width = 10, units = "in")


# Figure 2 interpretation -------------------------------------------------

# other plots and calculations to assist with interpreting figure 2

# numbered cause labelling
plota <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_codn),
         Region = factor(Region, levels = v_reg_dths),
         label_text = stringr::str_extract(COD, "^[^.]+")) %>%
  mutate(CSMF_pct   = CSMF * 100,
    label_text = ifelse(CSMF >= 0.01, label_text, "")) %>%
  filter(Year == 2024) %>%
  filter(Region != "World") %>%
  ggplot(aes(x = Region, y = CSMF_pct, fill = COD)) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),
            size = 3) +
  theme(text = element_text(size = 12)) +
  labs(title = "5-19y", y = "CSMF (%)", x= "") +
  coord_flip()
# percents in each cause labelling
plotb <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod),
         Region = factor(Region, levels = v_reg_dths)) %>%
  mutate(CSMF_pct   = CSMF * 100, CSMFround  = round(CSMF_pct, 1),
    label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "") ) %>%
  filter(Year == 2024) %>%
  filter(Region != "World") %>%
  ggplot(aes(x = Region, y = CSMF_pct, fill = COD)) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),
            size = 3) +
  theme(text = element_text(size = 12), legend.position = "none") +
  labs(title = "5-19y", y = "CSMF (%)", x= "") +
  coord_flip()
grid.arrange(plota, plotb, ncol = 2)


# aggregate by cod groupings
aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  left_join(df_codgrp, by = "COD") %>%
  mutate(Region = factor(Region, levels = v_reg_dths)) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  group_by(Region, CODgrp, Deaths) %>%
  summarise(CSMF = sum(CSMF)) %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  pivot_longer(
    cols = c(Deaths, CSMF),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(label_text = ifelse(Variable == "Deaths", "", round(Value*100,1))) %>%
  ggplot(aes(x = Region, y = Value, fill = CODgrp)) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  geom_text(aes(label = label_text, group = CODgrp),
            position = position_stack(vjust = 0.5),size = 3) +
  facet_wrap(~Variable, scales = "free_x") +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "bottom", legend.title = element_blank())

# number of non-communicable deaths in top regions
aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(Region != "World" & Year == 2024 & CODgrp == "ncd") %>%
  group_by(Region, CODgrp, Deaths) %>%
  summarise(CSMF = sum(CSMF)) %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  arrange(-Deaths)

# leading non-communicable diseases in top regions
aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(Region != "World" & Year == 2024 & CODgrp == "ncd") %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  group_by(Region) %>%
  mutate(ncdtotal = sum(Deaths)) %>%
  arrange(-ncdtotal, -Deaths) # %>% View()

# rti
aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  filter(Region != "World" & Year == 2024 & COD == "RTI") %>%
  arrange(-CSMF) 

# Figure 3: regional cause distribution by age-sex group 2024 --------------------------

# CSMFs
p <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  ggplot(aes(x=AgeSexSuffix, y = CSMF, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  labs(x = "") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20)) +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())
ggsave(paste("./gen/visualizations/output/csmf_reg_byage_2024.png", sep=""), p, dpi = 500, height = 10, width = 8, units = "in")

# Deaths
p <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg_dths)) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  mutate(Deaths = CSMF*Deaths) %>%
  ggplot(aes(x=AgeSexSuffix, y = Deaths, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  labs(x = "") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20),
             scale= "free_y") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())
ggsave(paste("./gen/visualizations/output/dth_reg_byage_2024.png", sep=""), p, dpi = 500, height = 10, width = 8, units = "in")


# Figure 3 interpretation -------------------------------------------------

# other plots and calculations to assist with interpreting figure 2

# two panel plot with death counts, numbered causes and percents in each cause
plota <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_codn),
         Region = factor(Region, levels = v_reg_dths),
         label_text = stringr::str_extract(COD, "^[^.]+")) %>%
  mutate(label_text = ifelse(CSMF >= 0.07, label_text, "")) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  mutate(Deaths = CSMF*Deaths) %>%
  ggplot(aes(x=AgeSexSuffix, y = Deaths, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(x = "") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20),
             scale= "free_y") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())
plotb <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_codn),
         Region = factor(Region, levels = v_reg_dths)) %>%
  mutate(CSMF_pct   = CSMF * 100, CSMFround  = round(CSMF_pct, 1),
         label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "") ) %>%
  mutate(label_text = ifelse(CSMF >= 0.07, label_text, "")) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  mutate(Deaths = CSMF*Deaths) %>%
  ggplot(aes(x=AgeSexSuffix, y = Deaths, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(x = "") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20),
             scale= "free_y") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(), legend.position = "none")
grid.arrange(plota, plotb, ncol = 2)

# two panel plot with CSMFs, numbered causes and percents in each cause
plota <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_codn),
         Region = factor(Region, levels = v_reg_dths),
         label_text = stringr::str_extract(COD, "^[^.]+")) %>%
  mutate(label_text = ifelse(CSMF >= 0.07, label_text, "")) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  mutate(Deaths = CSMF*Deaths) %>%
  ggplot(aes(x=AgeSexSuffix, y = CSMF, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(x = "") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20),
             scale= "free_y") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())
plotb <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_codn),
         Region = factor(Region, levels = v_reg_dths)) %>%
  mutate(CSMF_pct   = CSMF * 100, CSMFround  = round(CSMF_pct, 1),
         label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "") ) %>%
  mutate(label_text = ifelse(CSMF >= 0.07, label_text, "")) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  mutate(Deaths = CSMF*Deaths) %>%
  ggplot(aes(x=AgeSexSuffix, y = CSMF, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(x = "") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20),
             scale= "free_y") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(), legend.position = "none")
grid.arrange(plota, plotb, ncol = 2)


# age distribution of deaths by region
allReg %>%
  mutate(Region = factor(Region, levels = v_reg_dths)) %>%
  filter(Year == 2024) %>%
  select(AgeGroup, AgeSexSuffix, Region, Year, Deaths) %>%
  group_by(Region, AgeGroup) %>%
  summarise(Deaths = sum(Deaths)) %>%
  group_by(Region) %>%
  mutate(per = round(Deaths/sum(Deaths)*100, 1)) %>%
  as.data.frame()
# highest burden countries deaths 5-9 years (call out #1, Nigeria)
nat05to09 %>%
  filter(Year == 2024) %>%
  arrange(-Deaths) %>%
  select(Region, ISO3, Deaths) %>%
  head(10)
# highest burden countries deaths 10-14 years (call out #1, India)
nat10to14 %>%
  filter(Year == 2024) %>%
  arrange(-Deaths) %>%
  select(Region, ISO3, Deaths) %>%
  head(10)

# highest levels of certain causes
fig3num <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg_dths)) %>%
  filter(Region != "World" & Year == 2024) %>%
  mutate(CSD = CSMF*Deaths) %>%
  select(-c(AgeGroup, Sex, Rate))

# othercmpn
# highest othercmpn conditions cause group by age-sex group and region (call out high levels of communicable diseases in west African 5-14 year olds)
fig3num %>%
  left_join(df_codgrp, by = "COD") %>%
  group_by(AgeSexSuffix, Region, CODgrp) %>%
  summarise(CSMF = sum(CSMF, na.rm = TRUE)) %>%
  arrange(-CSMF) %>%
  filter(CODgrp == "communicable") %>%
  head(8)
# leading communicable causes west/central africa 5-9 (call out top 3)
fig3num %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(CODgrp == "communicable") %>%
  filter(Region == "West and central Africa" &
           AgeSexSuffix %in% c("05to09y") & COD != "OtherCMPN") %>%
  arrange(-CSMF) %>%
  head(3)
# leading communicable causes west/central africa 10-14 (call out top 3)
fig3num %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(CODgrp == "communicable") %>%
  filter(Region == "West and central Africa" &
           AgeSexSuffix %in% c("10to14y") & COD != "OtherCMPN") %>%
  arrange(-CSMF) %>%
  head(3)
# leading communicable causes west/central africa 5-9 (call out top 3)
fig3num %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(CODgrp == "communicable") %>%
  filter(Region == "South Asia" &
           AgeSexSuffix %in% c("05to09y") & COD != "OtherCMPN") %>%
  arrange(-CSMF) %>%
  head(3)

# maternal
fig3num %>%
  filter(AgeSexSuffix == "15to19yF" & COD == "Maternal") %>%
  arrange(-CSMF)
# highest csmfs by country (call out top 3)
nat15to19f %>%
  select(Region, ISO3, Year, Deaths, Rate, Maternal) %>%
  filter(Year == 2024) %>%
  arrange(-Maternal) %>%
  head(5)
# highest burden by country (call out top 3)
nat15to19f %>%
  select(Region, ISO3, Year, Deaths, Rate, Maternal) %>%
  filter(Year == 2024) %>%
  mutate(CSD = Maternal*Deaths) %>%
  arrange(-CSD) %>%
  head(10)

# tuberculosis
fig3num %>%
  filter(COD == "TB") %>%
  arrange(-CSMF)
# highest deaths by age-sex group/country (call out india and pakistan)
allNat %>%
  select(AgeSexSuffix, Region, ISO3, Year, Deaths, Rate, TB) %>%
  filter(Year == 2024) %>%
  mutate(CSD = TB*Deaths) %>%
  arrange(-CSD) %>%
  head(7)
# highest csmfs by age-sex groups/countries (call out those over 20%)
allNat %>%
  select(AgeSexSuffix, Region, ISO3, Year, Deaths, Rate, TB) %>%
  filter(Year == 2024) %>%
  arrange(-TB) %>%
  head(6)

# HIV
fig3num %>%
  filter(COD == "HIV") %>%
  arrange(-CSMF)
# highest deaths by country (call out top 3 and look at within-country differences in age group rank)
allNat %>%
  select(AgeSexSuffix, Region, ISO3, Year, Deaths, Rate, HIV) %>%
  filter(Year == 2024) %>%
  mutate(CSD = HIV*Deaths) %>%
  group_by(ISO3) %>%
  mutate(HIVtotal = sum(CSD),
         agePer = round(CSD/HIVtotal*100,1)) %>%
  arrange(-HIVtotal, -Deaths) %>%
  head(12)

# collective violence
fig3num %>%
  filter(COD == "CollectVio") %>%
  arrange(-CSMF)
# highest csmfs by age-sex groups/countries (call out palestine and ukraine)
allNat %>%
  select(AgeSexSuffix, Region, ISO3, Year, Deaths, Rate, CollectVio) %>%
  filter(Year == 2024) %>%
  arrange(-CollectVio) %>%
  head(6)

# highest injury conditions cause group by age-sex group and region (call out high levels of injuries)
fig3num %>%
  left_join(df_codgrp, by = "COD") %>%
  group_by(AgeSexSuffix, Region, CODgrp) %>%
  summarise(CSMF = sum(CSMF, na.rm = TRUE)) %>%
  arrange(-CSMF) %>%
  filter(CODgrp == "injury") %>%
  head(8)
# leading injury causes latin america 15-19m (call out top 3)
fig3num %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(CODgrp == "injury") %>%
  filter(Region == "Latin America and Caribbean" &
           AgeSexSuffix %in% c("15to19yM")) %>%
  arrange(-CSMF) %>%
  head(3)
fig3num %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(CODgrp == "injury") %>%
  filter(Region == "Eastern Europe and central Asia" &
           AgeSexSuffix %in% c("15to19yM")) %>%
  arrange(-CSMF) %>%
  head(3)
fig3num %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(CODgrp == "injury") %>%
  filter(Region == "North America" &
           AgeSexSuffix %in% c("15to19yM")) %>%
  arrange(-CSMF) %>%
  head(3)

# top 3 leading causes for 15-19f years by region (call out selfharm being top 3 in all but west/central africa)
fig3num %>%
  filter(AgeSexSuffix == "15to19yF") %>%
  arrange(-CSMF) %>%
  group_by(Region) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 3) %>%
  arrange(Region, -CSMF) %>%
  data.frame()
fig3num %>%
  filter(AgeSexSuffix == "15to19yM") %>%
  arrange(-CSMF) %>%
  group_by(Region) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 3) %>%
  arrange(Region, -CSMF) %>%
  data.frame()

# Figure 4: average annual rate of reduction ------------------------------

# cause-specific mortality rates
plotdat1 <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(
    COD = factor(COD, levels = v_cod, labels = v_cod),
    Region = factor(Region, levels = v_reg_dths)
  ) %>%
  filter(COD != "NatDis") %>%
  mutate(CSMR = CSMF * Rate)

# aarr by period
plotdat2 <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(
    COD = factor(COD, levels = v_cod, labels = v_cod),
    Region = factor(Region, levels = v_reg_dths)
  ) %>%
  filter(COD != "NatDis") %>%
  mutate(CSMR = CSMF * Rate,
         period = ifelse(Year %in% 2000:2015, "2000-2015", "2016-2024"))
# include 2015 in both periods
plotdat2 <- plotdat2 %>%
  bind_rows(plotdat2 %>% filter(Year == 2015) %>% mutate(period = "2016-2024")) %>%
  mutate(period = factor(period, levels = c("2000-2015", "2016-2024"))) %>%
  arrange(Region, COD, Year) %>%
  group_by(Region, COD) %>%
  mutate(lagCSMR = lag(CSMR),
         RR = lagCSMR - CSMR) %>%
  group_by(Region, COD, period) %>%
  summarise(AARR = mean(RR, na.rm = TRUE)) %>%
  mutate(AARRstart = ifelse(period == "2000-2015", AARR, NA),
         AARRend = ifelse(period == "2016-2024", AARR, NA),
         AARRend = max(AARRend, na.rm = TRUE),
         AARRend = ifelse(period == "2016-2024", NA, AARRend)) 


# set cod colors
mycols <- hue_pal()(length(v_cod))
cod_colors <- setNames(mycols, v_cod)

l_plota <- list()
l_plotb <- list()
for(i in 1:length(v_reg_dths)){
  
  v_cod_order <- plotdat1 %>%
    filter(Region %in% v_reg_dths[i]) %>%
    filter(Year == 2024) %>%
    arrange(CSMR) %>%
    select(COD) %>% pull()
  
  l_plota[[i]] <- plotdat1 %>%
    filter(Region %in% v_reg_dths[i]) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = 2015, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
    annotate("rect", xmin = 2015, xmax = Inf, ymin = -Inf, ymax = Inf,fill = "white", alpha = 1) +
    geom_line(aes(x = Year, y = CSMR, color = COD)) +
    geom_vline(aes(xintercept = 2015)) +
    scale_color_manual(values = cod_colors) +
    scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020, 2024)) +
    theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
  
  l_plotb[[i]] <- plotdat2 %>%
    filter(Region %in% v_reg_dths[i]) %>%
    mutate(COD = factor(COD, levels = v_cod_order)) %>%
    ggplot() +
    geom_point(aes(x = COD, y = AARR, shape = period, color = COD)) +
    geom_segment(aes(x = COD, xend = COD, y = AARRstart, yend = AARRend, color = COD), 
                 arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
                 show.legend = FALSE) +
    geom_hline(aes(yintercept = 0)) +
    labs(x = "")+
    scale_shape_manual(values = c(16, NA)) +
    scale_color_manual(values = cod_colors) +
    guides(shape  = "none", color = "none") +
    theme(plot.margin = margin(0, 0, 0, 0) #,
          #text = element_text(size = 8) # smaller text for plot with 5 regions
          ) +
    coord_flip() 
}

p1 <- l_plota[[2]] + l_plotb[[2]] + plot_layout(guides = "collect")
p1 <- p1 + plot_annotation(subtitle = v_reg_dths[2])
p2 <- l_plota[[3]] + l_plotb[[3]] + plot_layout(guides = "collect")
p2 <- p2 + plot_annotation(subtitle = v_reg_dths[3])
p3 <- l_plota[[4]] + l_plotb[[4]] + plot_layout(guides = "collect")
p3 <- p3 + plot_annotation(subtitle = v_reg_dths[4])
p4 <- l_plota[[5]] + l_plotb[[5]] + plot_layout(guides = "collect")
# Add legend to bottom
dummy_plot <- ggplot() +
  dummy_guide(
    labels = c("2000–2015", "2016–2024"),
    shape  = c(16, 17),
    title = "Period"
  ) +
  theme_void() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.direction = "horizontal",
        plot.margin = margin(0, 0, 0, 0))
p4 <- p4  / dummy_plot + plot_layout(heights = c(1, 0.1)) + plot_annotation(subtitle = v_reg_dths[5])
aar1 <- (wrap_elements(p1) / wrap_elements(p2) / wrap_elements(p3) /  wrap_elements(p4)) +
  plot_layout(heights = c(1, 1, 1, 1.1)) &
  theme(plot.margin = margin(-1, -1, -1, -1))
ggsave("./gen/visualizations/output/aar1.png", aar1, width = 8, height = 10, dpi = 500)

p5 <- l_plota[[6]] + l_plotb[[6]] + plot_layout(guides = "collect")
p5 <- p5 + plot_annotation(subtitle = v_reg_dths[6])
p6 <- l_plota[[7]] + l_plotb[[7]] + plot_layout(guides = "collect")
p6 <- p6 + plot_annotation(subtitle = v_reg_dths[7])
p7 <- l_plota[[8]] + l_plotb[[8]] + plot_layout(guides = "collect")
p7 <- p7 + plot_annotation(subtitle = v_reg_dths[8])
p8 <- l_plota[[9]] + l_plotb[[9]] + plot_layout(guides = "collect")
p8 <- p8 + plot_annotation(subtitle = v_reg_dths[9])
p9 <- l_plota[[10]] + l_plotb[[10]] + plot_layout(guides = "collect")
dummy_plot <- ggplot() +
  dummy_guide(
    labels = c("2000–2015", "2016–2024"),
    shape  = c(16, 17),
    title = "Period"
  ) +
  theme_void() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.direction = "horizontal",
        plot.margin = margin(0, 0, 0, 0))
p9 <- p9  / dummy_plot + plot_layout(heights = c(1, 0.1)) + plot_annotation(subtitle = v_reg_dths[10])
aar2 <- (wrap_elements(p5) / wrap_elements(p6) / wrap_elements(p7) /  wrap_elements(p8) /  wrap_elements(p9)) +
  plot_layout(heights = c(1, 1, 1, 1, 1.075)) &
  theme(plot.margin = margin(-1, -1, -1, -1))
ggsave("./gen/visualizations/output/aar2.png", aar2, width = 8, height = 10, dpi = 500)


# Figure 4 interpretation -------------------------------------------------

# aarrs
v_reg_dths
plotdat2 %>%
  filter(Region %in% c("West and central Africa", "Eastern and Southern Africa" )) %>%
  select(Region, COD, period, AARR) %>%
  pivot_wider(names_from = period, values_from = AARR)

# csmr
# call out West and central Africa decrease in measles
plotdat1 %>%
  filter(Region %in% c("West and central Africa","Eastern and Southern Africa" )) %>%
  filter(COD == "Measles") %>%
  filter(Year %in% c(2000, 2015, 2024)) %>%
  select(Region, Year, COD, CSMR)

# increase in collective violence in later period in east asia and pacific
# call out Myanmar
aggNat5to19 %>%
  filter(Region == "East Asia and Pacific") %>%
  filter(Year >= 2015 & CollectVio > 0) %>%
  select(ISO3, Year, CollectVio)

# Interpersonal violence in latin america and caribbean
plotdat2 %>%
  filter(Region %in% c("Latin America and Caribbean")) %>%
  filter(COD == "InterpVio") %>%
  select(Region, COD, period, AARR) %>%
  pivot_wider(names_from = period, values_from = AARR)

# collective violence in middle east and north africa, call out year of increase in period 2015-2024 (2023)
plotdat1 %>%
  filter(Region %in% c("Middle East and North Africa" )) %>%
  filter(COD == "CollectVio") %>%
  select(Region, Year, COD, CSMR) %>%
  data.frame()


# Old numbers for paper -------------------------------------------------------

# Deaths in 2024
aggReg5to19 %>%
  filter(Region == "World" & Year == 2024) %>%
  select(Deaths) %>%
  
# Number of HMM, LMM, VR countries
length(unique(nat05to09$ISO3)) # 195
nat05to09 %>%
  select(ISO3, Model) %>%
  distinct() %>%
  group_by(Model) %>%
  summarise(n = n())

# Leading causes in 5-19 in 2024
aggReg5to19 %>%
  filter(Region == "World" & Year == 2024) %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  arrange(-CSMF) %>%
  mutate(per = Deaths*CSMF)
aggReg5to19 %>%
  filter(Region == "World" & Year == 2024) %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  filter(!(COD %in% c("OtherCMPN", "OtherNCD", "OtherInj"))) %>%
  arrange(-CSMF)

# top 5 causes in 2024 by age
allReg %>%
  filter(Region == "World" & Year == 2024) %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  group_by(AgeSexSuffix) %>%
  arrange(AgeSexSuffix, -CSMF) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 5)

# top five burden countries in 5-19
aggNat5to19 %>%
  filter(Year == 2024) %>%
  arrange(-Deaths) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 5) %>%
  select(WHOname, Year, Deaths)

# top five burden countries in each age group
allNat %>%
  select(-c(ISO3, AgeLow, AgeUp, Sex, Model, FragileState, SDGregion, UNICEFReportRegion1, UNICEFReportRegion2)) %>%
  relocate(AgeSexSuffix, everything()) %>%
  filter(Year == 2024) %>%
  group_by(AgeSexSuffix) %>%
  arrange(AgeSexSuffix, -Deaths) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 5) %>%
  select(AgeSexSuffix, WHOname, Year, Deaths)

# deaths by age group
nat05to09 %>% 
  bind_rows(nat10to14, nat15to19f, nat15to19m) %>%
  filter(Year == 2024) %>%
  group_by(AgeSexSuffix) %>%
  summarise(Deaths = sum(Deaths)) %>%
  mutate(total = sum(Deaths), 
         per = Deaths/total,
         AgeGroup = ifelse(AgeSexSuffix %in% c("15to19yF", "15to19yM"), "15to19y", AgeSexSuffix)) %>%
  group_by(AgeGroup) %>%
  mutate(Deaths2 = sum(Deaths),
         per2 = Deaths2/total)

# male/female death split in 15-19
nat15to19f %>%
  bind_rows(nat15to19m) %>%
  filter(Year == 2024) %>%
  group_by(AgeSexSuffix) %>%
  summarise(Deaths = sum(Deaths)) %>%
  mutate(total = sum(Deaths), 
         per = Deaths/total)

# deaths by age group and region
reg05to09 %>% 
  bind_rows(reg10to14, reg15to19f, reg15to19m) %>%
  filter(Year == 2024, Region != "World") %>%
  group_by(AgeSexSuffix, Region) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>% 
  group_by(Region) %>%
  mutate(total = sum(Deaths),
         per = Deaths / total) %>%
  ungroup() %>%
  mutate(Region = factor(Region, levels = v_reg),
         label = round(per*100, 1)) %>%
  ggplot(aes(x = Region, y = per, fill = AgeSexSuffix)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 3) +
  coord_flip() 

# deaths by age group and model
nat05to09 %>% 
  bind_rows(nat10to14, nat15to19f, nat15to19m) %>%
  filter(Year == 2024) %>%
  left_join(key_ctryclass %>% select(iso3, Group2010), by = c("ISO3" = "iso3")) %>%
  group_by(AgeSexSuffix, Group2010) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>% 
  group_by(Group2010) %>%
  mutate(total = sum(Deaths),
         per = Deaths / total) %>%
  ungroup() %>%
  mutate(label = round(per*100, 1)) %>%
  ggplot(aes(x = Group2010, y = per, fill = AgeSexSuffix)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 3) +
  coord_flip()
# combine 15-19
nat05to09 %>% 
  bind_rows(nat10to14, nat15to19f, nat15to19m) %>%
  filter(Year == 2024) %>%
  left_join(key_ctryclass %>% select(iso3, Group2010), by = c("ISO3" = "iso3")) %>%
  mutate(AgeSexSuffix = ifelse(AgeSexSuffix %in% c("15to19yF", "15to19yM"), "15to19y", AgeSexSuffix)) %>%
  group_by(AgeSexSuffix, Group2010) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>% 
  group_by(Group2010) %>%
  mutate(total = sum(Deaths),
         per = Deaths / total) %>%
  ungroup() %>%
  mutate(label = round(per*100, 1)) %>%
  ggplot(aes(x = Group2010, y = per, fill = AgeSexSuffix)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 3) +
  coord_flip() 


# Figure 3 alternatives --------------------------------------------------

panelA <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  ggplot(aes(x=AgeSexSuffix, y = CSMF, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  labs(x = "") +
  facet_wrap(~Region, ncol = 1) +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())
panelB <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  mutate(Deaths = CSMF * Deaths) %>%
  ggplot(aes(x=AgeSexSuffix, y = Deaths, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  labs(x = "") +
  facet_wrap(~Region, ncol = 1, scales = "free_y") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# remove legend from one of the panels
panelA_noleg <- panelA + theme(legend.position = "none")
# combine vertically with shared legend on the right (or bottom)
combined_plot <- panelA_noleg / panelB + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")  # or "right"
combined_plot <- 
  (panelA + theme(legend.position = "none")) |   # place side by side
  panelB + 
  plot_layout(guides = "collect") &              # combine shared legend
  theme(legend.position = "bottom")              # move legend to bottom
combined_plot


allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  mutate(Deaths = CSMF*Deaths) %>%
  ggplot(aes(x=AgeSexSuffix, y = Deaths, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  labs(x = "") +
  facet_wrap(~Region, scales = "free_y",  
             labeller = label_wrap_gen(width = 20)) +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  mutate(Deaths = CSMF*Deaths) %>%
  ggplot(aes(x=Region, y = Deaths, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  labs(x = "") +
  facet_wrap(~AgeSexSuffix,  
             labeller = label_wrap_gen(width = 20),
             ncol = 1) +
  coord_flip() +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())


allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World") %>%
  filter(Year == 2024) %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  pivot_longer(
    cols = c(Deaths, CSMF),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  ggplot(aes(x=Region, y = Value, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  labs(x = "") +
  facet_grid(AgeSexSuffix ~ Variable, scales = "free_x") +
  coord_flip() +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())


allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(
    COD = factor(COD, levels = v_cod, labels = v_cod),
    Region = factor(Region, levels = v_reg)
  ) %>%
  filter(Region != "World", Year == 2024) %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF * env) %>%
  pivot_longer(
    cols = c(Deaths, CSMF),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    facet_label = paste0(Region, " – ", Variable)
  ) %>%
  ggplot(aes(x = AgeSexSuffix, y = Value, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  labs(x = "") +
  facet_wrap(
    ~ facet_label,
    ncol = 2,     
    scales = "free_y"
  ) +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    strip.text = element_text(size = 11)
  )



# Regional CSMFs 5-19y 2000-2024 ------------------------------------------------

p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_codn),
         Region = factor(Region, levels = v_reg),
         label_text = stringr::str_extract(COD, "^[^.]+")) %>%
  mutate(label_text = ifelse(CSMF >= 0.03, label_text, "")) %>%
  ggplot(aes(x=Year, y = CSMF, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),   # centers in each stacked segment
            size = 1) +
  labs(title = "5-19y") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20), nrow = 3) +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave(paste("./gen/visualizations/output/csmf_reg_05to19.png", sep=""), p, dpi = 500, height = 10, width = 7, units = "in")


# Regional CSMFs by COD group 5-19y 2000-2024 -----------------------------

p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  left_join(df_codgrp, by = "COD") %>%
  group_by(Region, Year, CODgrp) %>%
  summarise(CSMF = sum(CSMF)) %>%
  mutate(
    Region = factor(Region, levels = v_reg),
    CSMF_pct   = CSMF * 100,      # convert to percent for both bars and labels
    CSMFround  = round(CSMF_pct, 0),
    label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "") # hide tiny labels
  ) %>%
  ggplot(aes(x=Year, y = CSMF, fill = CODgrp)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label_text, group = CODgrp),
            position = position_stack(vjust = 0.5),   # centers in each stacked segment
            size = 1.5) +
  labs(title = "5-19y") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20), nrow = 3) +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave(paste("./gen/visualizations/output/csmf-grp_reg_05to19.png", sep=""), p, dpi = 500, height = 10, width = 7, units = "in")



# Regional CSMRs by COD group 5-19y 2000-2024 -----------------------------


p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(CSMR = CSMF * Rate) %>%
  left_join(df_codgrp, by = "COD") %>%
  group_by(Region, Year, CODgrp) %>%
  summarise(CSMR = sum(CSMR)) %>%
  mutate(
    Region = factor(Region, levels = v_reg),
  ) %>%
  ggplot(aes(x=Year, y = CSMR, color = CODgrp)) +
  geom_line() +
  labs(title = "5-19y") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20), nrow = 3) +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave(paste("./gen/visualizations/output/csmr-grp_reg_05to19.png", sep=""), p, dpi = 500, height = 10, width = 7, units = "in")

p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(CSMR = CSMF * Rate) %>%
  left_join(df_codgrp, by = "COD") %>%
  group_by(Region, Year, CODgrp) %>%
  summarise(CSMR = sum(CSMR)) %>%
  mutate(
    Region = factor(Region, levels = v_reg),
  ) %>%
  ggplot(aes(x=Year, y = CSMR, color = CODgrp)) +
  geom_line() +
  labs(title = "5-19y") +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20), nrow = 3, scales = "free_y") +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave(paste("./gen/visualizations/output/csmr-grp2_reg_05to19.png", sep=""), p, dpi = 500, height = 10, width = 7, units = "in")




# Regional CSMFs 5-19y 2024 -----------------------------------------------

p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  mutate(
    CSMF_pct   = CSMF * 100,      # convert to percent for both bars and labels
    CSMFround  = round(CSMF_pct, 0),
    label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "") # hide tiny labels
  ) %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = Region, y = CSMF_pct, fill = COD)) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),   # centers in each stacked segment
            size = 3) +
  theme(text = element_text(size = 12)) +
  labs(title = "5-19y", y = "CSMF (%)") +
  coord_flip()
ggsave(paste("./gen/visualizations/output/csmf_reg_05to19_2024.png", sep=""), p, dpi = 500, height = 6, width = 10, units = "in")

# with COD n labelling
p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_codn),
         Region = factor(Region, levels = v_reg),
         label_text = stringr::str_extract(COD, "^[^.]+")) %>%
  mutate(
    CSMF_pct   = CSMF * 100,      # convert to percent for both bars and labels
    label_text = ifelse(CSMF >= 0.01, label_text, "") # hide tiny labels
  ) %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = Region, y = CSMF_pct, fill = COD)) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),   # centers in each stacked segment
            size = 3) +
  theme(text = element_text(size = 12)) +
  labs(title = "5-19y", y = "CSMF (%)") +
  coord_flip()
ggsave(paste("./gen/visualizations/output/csmf_reg_05to19_2024_codlab.png", sep=""), p, dpi = 500, height = 6, width = 10, units = "in")


# Regional ----------------------------------------------------------------

p <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World") %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  pivot_longer(
    cols = c(Deaths, CSMF),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = Region, y = Value, fill = COD)) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  facet_wrap(~Variable, scales = "free_x") +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "", x= "", y = "")
ggsave(paste("./gen/visualizations/output/csmf_deaths_reg_05to19_2024.png", sep=""), p, dpi = 500, height = 6, width = 10, units = "in")

# Region-level 5-19, 2000 and 2024 ----------------------------------------

aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  mutate(
    CSMF_pct   = CSMF * 100,      # convert to percent for both bars and labels
    CSMFround  = round(CSMF_pct, 0),
    label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "") # hide tiny labels
  ) %>%
  filter(Year %in% c(2000,2024) ) %>%
  ggplot(aes(x =Year, y = CSMF_pct, fill = COD)) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),   # centers in each stacked segment
            size = 3) +
  facet_wrap(~Region) +
  theme(text = element_text(size = 12)) +
  labs(title = "5-19y", y = "CSMF (%)") 



# Regional CSMFs by age group 2024 ----------------------------------------

p <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_codn),
         Region = factor(Region, levels = v_reg),
         label_text = stringr::str_extract(COD, "^[^.]+")) %>%
  filter(Region != "World") %>%
  mutate(label_text = ifelse(CSMF >= 0.01, label_text, "")) %>%
  filter(Year == 2024) %>%
  ggplot(aes(x=AgeSexSuffix, y = CSMF, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),   # centers in each stacked segment
            size = 2) +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20)) +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste("./gen/visualizations/output/csmf_reg_byage_2024.png", sep=""), p, dpi = 500, height = 10, width = 8, units = "in")


p <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = v_cod, labels = v_cod),
         Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World") %>%
  mutate(
    CSMF_pct   = CSMF * 100,      # convert to percent for both bars and labels
    CSMFround  = round(CSMF_pct, 0),
    label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "") # hide tiny labels
  ) %>%
  filter(Year == 2024) %>%
  ggplot(aes(x=AgeSexSuffix, y = CSMF, fill = COD)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = label_text, group = COD),
            position = position_stack(vjust = 0.5),   # centers in each stacked segment
            size = 2) +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20)) +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste("./gen/visualizations/output/csmf_reg_byage_2024b.png", sep=""), p, dpi = 500, height = 10, width = 8, units = "in")


# Regional grouped CSMFs by age group 2024 ----------------------------------------

p <- allReg %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  left_join(df_codgrp, by = "COD") %>%
  filter(Year == 2024) %>%
  group_by(AgeSexSuffix, Region, CODgrp) %>%
  summarise(CSMF = sum(CSMF, na.rm = TRUE)) %>%
  mutate(Region = factor(Region, levels = v_reg)) %>%
  filter(Region != "World" ) %>%
  mutate(
    CSMF_pct   = CSMF * 100,      # convert to percent for both bars and labels
    CSMFround  = round(CSMF_pct, 0),
    label_text = ifelse(CSMF_pct >= 1, as.character(CSMFround), "") # hide tiny labels
  ) %>%
  ggplot(aes(x=AgeSexSuffix, y = CSMF, fill = CODgrp)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = label_text, group = CODgrp),
            position = position_stack(vjust = 0.5),   # centers in each stacked segment
            size = 2) +
  facet_wrap(~Region,  labeller = label_wrap_gen(width = 20)) +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste("./gen/visualizations/output/csmf-grp_reg_byage_2024.png", sep=""), p, dpi = 500, height = 10, width = 8, units = "in")

# Old average annual rate of reduction ------------------------------

plotdat1 <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(
    COD = factor(COD, levels = v_cod, labels = v_cod),
    Region = factor(Region, levels = v_reg_dths)
  ) %>%
  filter(COD != "NatDis") %>%
  mutate(CSMR = CSMF * Rate)
plotdat2 <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(
    COD = factor(COD, levels = v_cod, labels = v_cod),
    Region = factor(Region, levels = v_reg_dths)
  ) %>%
  filter(COD != "NatDis") %>%
  mutate(CSMR = CSMF * Rate,
         period = ifelse(Year %in% 2000:2015, "2000-2014", "2015-2024")) %>%
  mutate(period = factor(period, levels = c("2000-2014", "2015-2024"))) %>%
  arrange(Region, COD, Year) %>%
  group_by(Region, COD) %>%
  mutate(lagCSMR = lag(CSMR),
         RR = lagCSMR - CSMR) %>%
  group_by(Region, COD, period) %>%
  summarise(AARR = mean(RR, na.rm = TRUE)) %>%
  mutate(AARRstart = ifelse(period == "2000-2014", AARR, NA),
         AARRend = ifelse(period == "2015-2024", AARR, NA),
         AARRend = max(AARRend, na.rm = TRUE),
         AARRend = ifelse(period == "2015-2024", NA, AARRend)) 

aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(
    COD = factor(COD, levels = v_cod, labels = v_cod),
    Region = factor(Region, levels = v_reg_dths)
  ) %>%
  mutate(CSMR = CSMF * Rate,
         period = ifelse(Year %in% 2000:2015, "2000-2014", "2015-2024")) %>%
  arrange(Region, COD, Year) %>%
  group_by(Region, COD) %>%
  mutate(lagCSMR = lag(CSMR),
         RR = lagCSMR - CSMR) %>%
  group_by(Region, COD, period) %>%
  summarise(AARR = mean(RR, na.rm = TRUE)) %>%
  ggplot(aes(x = COD, y = AARR, color = period)) +
  #geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~Region) +
  coord_flip() 


p1 <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(
    COD = factor(COD, levels = v_cod, labels = v_cod),
    Region = factor(Region, levels = v_reg_dths)
  ) %>%
  filter(COD != "NatDis") %>%
  filter(Region %in% v_reg[1:4]) %>%
  mutate(CSMR = CSMF * Rate) %>%
  ggplot() +
  geom_line(aes(x = Year, y = CSMR, color = COD)) +
  geom_vline(aes(xintercept = 2014)) +
  facet_wrap(~Region, ncol = 1, scales = "free_y") +
  theme(legend.position = "none")



p2 <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(
    COD = factor(COD, levels = v_cod, labels = v_cod),
    Region = factor(Region, levels = v_reg_dths)
  ) %>%
  filter(COD != "NatDis") %>%
  filter(Region %in% v_reg[1:4]) %>%
  mutate(CSMR = CSMF * Rate,
         period = ifelse(Year %in% 2000:2015, "2000-2014", "2015-2024")) %>%
  mutate(period = factor(period, levels = c("2000-2014", "2015-2024"))) %>%
  arrange(Region, COD, Year) %>%
  group_by(Region, COD) %>%
  mutate(lagCSMR = lag(CSMR),
         RR = lagCSMR - CSMR) %>%
  group_by(Region, COD, period) %>%
  summarise(AARR = mean(RR, na.rm = TRUE)) %>%
  mutate(AARRstart = ifelse(period == "2000-2014", AARR, NA),
         AARRend = ifelse(period == "2015-2024", AARR, NA),
         AARRend = max(AARRend, na.rm = TRUE),
         AARRend = ifelse(period == "2015-2024", NA, AARRend)) %>%
  ggplot() +
  geom_point(aes(x = COD, y = AARR, shape = period, color = COD)) +
  geom_segment(aes(x = COD, xend = COD, y = AARRstart, yend = AARRend, color = COD), 
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               show.legend = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "")+
  scale_shape_manual(values = c(16, NA)) +
  facet_wrap(~Region, ncol = 1) +
  guides(shape  = "none", color = "none") +
  coord_flip() 

p2 <- p2 + dummy_guide(
  labels = c("2000-2014", "2015-2024"), 
  shape  = c(16, 17),
  title = "Period"
)


p1 + p2 + plot_layout(ncol = 2)


