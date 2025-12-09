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
library(grid)
library(gridExtra)
library(patchwork)
library(scales)
library(forcats)
library(RColorBrewer)
library(scales)
#' Inputs
#' setwd('C:/Users/FVillavicencio/Dropbox/Mortality5to19/')
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

v_reg_lab <- c("Eastern and\nsouthern Africa", "West and\ncentral Africa", "Middle East\nand north Africa",
               "South Asia", "East Asia\nand Pacific", 
               "Latin America\nand Caribbean", "North America" ,
               "Eastern Europe\nand central Asia","Western Europe",
               "World")

v_reg_lab_nobreaks <- c("Eastern and southern Africa", "West and central Africa", "Middle East and north Africa",
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

# reorder labels
v_reg_lab <- v_reg_lab[match(v_reg_dths, v_reg)]
v_reg_lab_nobreaks <- v_reg_lab_nobreaks[match(v_reg_dths, v_reg)]

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
  geom_bar(color = "gray70",stat = "identity", position = "stack") +
  facet_wrap(~Variable, scales = "free_x") +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "bottom", legend.title = element_blank()) +
  theme_minimal() +
  labs(title = "", x= "", y = "")
ggsave(paste("./gen/visualizations/output/csmf_deaths_reg_05to19_2024.png", sep=""), p, dpi = 500, height = 6, width = 10, units = "in")

# Figure 2: updated cod labels and colors -------------------------------------------------------

# updated
# 1.	Color palette similar to the data portal.
# 2.	Same cause-labels as in data portal, with one exception: merge collective violence and natural disasters in one single category.
# 3.	Panel titles: “Mortality fractions (%)” and “Number of deaths”
# 4.	X-axis of the right-panel (fractions): I suggest values to range from 0 to 100 (with no decimal points) instead of 0 to 1.
# 5.	Legend: ideally, it should span the entire width of the figure, so no empty space on the sides. Perhaps we could distribute items in 3 lines instead of 4?
#   6.	With the new color palette, my guess is that the borders of the bars are no longer needed, neither in the graph nor in the legend.

options(scipen = 999)

# recode natdis and colvio
plotDat <- aggReg5to19 %>%
  mutate(ColVioNatDis = CollectVio + NatDis) %>%
  select(-c(CollectVio, NatDis))

# CODs in data portal order
v_cod_dp <- c("Diarrhoeal", "HIV",  "LRI", "Malaria", 
              "Maternal", "Measles" , "TB", "OtherCMPN", "Cardiovascular",
              "Congenital", "Digestive" ,  "Neoplasms" , "OtherNCD",
              "Drowning" , "RTI",  "SelfHarm" , "InterpVio" ,
              "ColVioNatDis", "OtherInj")
v_cod_lab_dp <- c('Diarrhea', 'HIV/AIDS', 'Lower respiratory\ninfections', 'Malaria',
         'Maternal causes', 'Measles', 'Tuberculosis', 'Other communicable\ndiseases', 
         'Cardiovascular', 'Congenital anomalies', 'Digestive system', 
         'Neoplasms/cancer', 'Other NCDs',
         'Drowning', 'Road traffic injuries', 'Self-harm', 
         'Interpersonal violence', 'Collective violence and\nnatural disasters',
         'Other injuries')

# Color palette 1: Communicable diseases of all ages
col1 <- brewer.pal(9, 'Oranges')[3:9]
col1 <- c(col1[1:4], 'plum1', col1[5:length(col1)])

# Color palette 2: Non-communicable diseases
col2 <- brewer.pal(6, 'Greens')[2:6]

# Color palette 4: Injuries
col3 <- brewer.pal(7, 'Blues')[2:7]

# Full palette
colPalette <- c(col1, col2, col3)
names(colPalette) <- v_cod_lab_dp

# uneven legend rows
p_main <- plotDat  %>%
  pivot_longer(
    cols = v_cod_dp,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = rev(v_cod_dp), labels = rev(v_cod_lab_dp)),
         Region = factor(Region, levels = v_reg_dths, v_reg_lab)) %>%
  filter(Region != "World") %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  mutate(CSMF = CSMF * 100) %>%
  pivot_longer(
    cols = c(Deaths, CSMF),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Variable = ifelse(Variable == "CSMF", "Mortality fractions (%)", "Number of deaths")) %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = Region, y = Value, fill = COD)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colPalette) +
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~Variable, scales = "free_x") +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(size = 14), panel.grid.major.y = element_blank(),
        legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "", x= "", y = "") +
  guides(fill = "none") 
legend_order <- plotDat  %>%
  pivot_longer(
    cols = v_cod_dp,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = rev(v_cod_dp), labels = rev(v_cod_lab_dp))) %>%
  select(COD) %>% pull()
causes_1 <- legend_order[1:8]
causes_2 <- legend_order[9:13]
causes_3 <- legend_order[14:18]
make_leg <- function(items, colorlow, colorhigh) {
  df <- data.frame(COD = factor(items, levels = items))
  
  ggplot(df, aes(x = COD, y = 1, fill = COD)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colPalette[colorlow:colorhigh], breaks = items) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = NULL)) +
    theme_void() +
    theme(legend.position = "bottom", text = element_text(size = 14))
}
leg1 <- cowplot::get_legend(make_leg(causes_1, 1, 8))
leg2 <- cowplot::get_legend(make_leg(causes_2, 9, 13))
leg3 <- cowplot::get_legend(make_leg(causes_3, 14, 18))
all_legs <- cowplot::plot_grid(
  leg1,
  leg2,
  leg3,
  ncol = 1,
  rel_heights = c(1,1,1)
)
p_final <- cowplot::plot_grid(
  p_main + guides(fill = "none"),
  all_legs,
  ncol = 1,
  rel_heights = c(1, 0.3)
)
p_final
ggsave(paste("./gen/visualizations/output/csmf_deaths_reg_05to19_2024_v2.png", sep=""), p_final, dpi = 500, height = 6, width = 12, units = "in")


# Figure 2: with annotations ---------------------------------------------

# CODs in data portal order
v_cod_dp <- c("Diarrhoeal", "HIV",  "LRI", "Malaria", 
              "Maternal", "Measles" , "TB", "OtherCMPN", "Cardiovascular",
              "Congenital", "Digestive" ,  "Neoplasms" , "OtherNCD",
              "Drowning" , "RTI",  "SelfHarm" , "InterpVio" ,
              "ColVioNatDis", "OtherInj")
v_cod_lab_dp <- c('Diarrhea', 'HIV/AIDS', 'Lower respiratory\ninfections', 'Malaria',
                  'Maternal causes', 'Measles', 'Tuberculosis', 'Other communicable\ndiseases', 
                  'Cardiovascular', 'Congenital anomalies', 'Digestive system', 
                  'Neoplasms/cancer', 'Other NCDs',
                  'Drowning', 'Road traffic injuries', 'Self-harm', 
                  'Interpersonal violence', 'Collective violence and\nnatural disasters',
                  'Other injuries')

# Color palette 1: Communicable diseases of all ages
col1 <- brewer.pal(9, 'Oranges')[3:9]
col1 <- c(col1[1:4], 'plum1', col1[5:length(col1)])

# Color palette 2: Non-communicable diseases
col2 <- brewer.pal(6, 'Greens')[2:6]

# Color palette 4: Injuries
col3 <- brewer.pal(7, 'Blues')[2:7]

# Full palette
colPalette <- c(col1, col2, col3)
names(colPalette) <- v_cod_lab_dp

plotDat1 <- aggReg5to19 %>%
  mutate(ColVioNatDis = CollectVio + NatDis) %>%
  select(-c(CollectVio, NatDis))

plotDat2 <- aggReg5to19 %>%
  mutate(ColVioNatDis = CollectVio + NatDis) %>%
  select(-c(CollectVio, NatDis)) %>%
  pivot_longer(
    cols = v_cod_dp,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = rev(v_cod_dp), labels = rev(v_cod_lab_dp)),
         Region = factor(Region, levels = v_reg_dths, v_reg_lab)) %>%
  filter(Region != "World") %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  mutate(CSMF = CSMF * 100) %>%
  pivot_longer(
    cols = c(Deaths, CSMF),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Variable = ifelse(Variable == "CSMF", "Mortality fractions (%)", "Number of deaths")) %>%
  filter(Year == 2024)
totals <-  plotDat2 %>%
  filter(Variable == "Number of deaths") %>%
  group_by(Region) %>%
  summarise(Total = sum(Value), .groups = "drop") %>%
  mutate(Variable = "Number of deaths") 
prob_region <- "West and\ncentral Africa"
prob_total <- totals %>% 
  filter(Region == prob_region)
anno_df <- totals %>%
  filter(Region == prob_region) %>%
  mutate(
    y_pos = Total, 
    label = scales::comma(round(Total, 0)),
    Variable = "Number of deaths"
  )
totals <- totals %>%
  filter(Region != "West and\ncentral Africa" )

p_main <- ggplot(plotDat2) +
  geom_bar(aes(x = Region, y = Value, fill = COD),
           stat = "identity", position = "stack") +
  geom_text(
    data = totals,
    aes(
      x = Region,
      y = Total,
      label = scales::comma(round(Total, 0))
    ),
    hjust = -0.1, 
    size = 4
  ) +
  geom_segment(
    data = prob_total,
    aes(x = 3.8, xend = 1.6, 
        y = prob_total %>% select(Total) %>% pull() * .98, 
        yend = prob_total %>% select(Total) %>% pull() * .98),
    #arrow = arrow(length = unit(0.15, "cm")),
    linewidth = 0.4
  ) +
  geom_text(
    data = anno_df,
    aes(
      x = 4,
      y = y_pos,
      label = label
    ),
    size = 4,
    hjust = 1
  ) +
  scale_fill_manual(values = colPalette) +
  scale_y_continuous(labels = label_comma()) + #, expand = expansion(mult = c(0, 0.1))) +
  facet_wrap(~Variable, scales = "free_x") +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(t = -10, r = 10, b = 0, l = 10)  
  ) +
  labs(title = "", x = "", y = "") +
  guides(fill = "none")


legend_order <- plotDat1  %>%
  pivot_longer(
    cols = v_cod_dp,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = rev(v_cod_dp), labels = rev(v_cod_lab_dp))) %>%
  select(COD) %>% pull()
causes_1 <- legend_order[1:8]
causes_2 <- legend_order[9:13]
causes_3 <- legend_order[14:18]
make_leg <- function(items, colorlow, colorhigh) {
  df <- data.frame(COD = factor(items, levels = items))
  
  ggplot(df, aes(x = COD, y = 1, fill = COD)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colPalette[colorlow:colorhigh], breaks = items) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = NULL)) +
    theme_void() +
    theme(legend.position = "bottom", text = element_text(size = 14),
          plot.margin = margin(t = -10, r = 10, b = -10, l = 10)  )
}
leg1 <- cowplot::get_legend(make_leg(causes_1, 1, 8))
leg2 <- cowplot::get_legend(make_leg(causes_2, 9, 13))
leg3 <- cowplot::get_legend(make_leg(causes_3, 14, 18))
all_legs <- cowplot::plot_grid(
  leg1,
  leg2,
  leg3,
  ncol = 1,
  rel_heights = c(1,1,1)
)
p_final <- cowplot::plot_grid(
  p_main + guides(fill = "none"),
  all_legs,
  ncol = 1,
  rel_heights = c(1, 0.3)
)
p_final

ggsave(paste("./gen/visualizations/output/csmf_deaths_reg_05to19_2024_v4.png", sep=""), p_final, dpi = 500, height = 6, width = 10, units = "in")

# Figure 2: final ---------------------------------------------

# CODs in data portal order
v_cod_dp <- c("Diarrhoeal", "HIV",  "LRI", "Malaria", 
              "Maternal", "Measles" , "TB", "OtherCMPN", "Cardiovascular",
              "Congenital", "Digestive" ,  "Neoplasms" , "OtherNCD",
              "Drowning" , "RTI",  "SelfHarm" , "InterpVio" ,
              "CollectVio", "NatDis", "OtherInj")

v_cod_lab_dp <- c('Diarrhea', 'HIV/AIDS', 'Lower respiratory\ninfections', 'Malaria',
                  'Maternal causes', 'Measles', 'Tuberculosis', 'Other communicable\ndiseases', 
                  'Cardiovascular', 'Congenital anomalies', 'Digestive system', 
                  'Neoplasms/cancer', 'Other NCDs',
                  'Drowning', 'Road traffic injuries', 'Self-harm', 'Interpersonal violence', 
                  'Collective violence', 'Natural disasters', 'Other injuries')

# Color palette 1: Communicable diseases of all ages
col1 <- brewer.pal(9, 'Oranges')[3:9]
col1 <- c(col1[1:4], 'plum1', col1[5:length(col1)])

# Color palette 2: Non-communicable diseases
col2 <- brewer.pal(6, 'Greens')[2:6]

# Color palette 4: Injuries
col3 <- brewer.pal(6, 'Blues')[2:6]
col3 <- c(col3[1:4], grey(.7), grey(.4), col3[length(col3)])

# Full palette
colPalette <- c(col1, col2, col3)
names(colPalette) <- v_cod_lab_dp

# Test
par(las = 1, mar = rep(1, 4))
plot(1:length(colPalette), pch = 19, cex = 2,
     col = colPalette, 
     xaxt = 'n', yaxt = 'n')
text(x = 1:length(colPalette),
     y = 1:length(colPalette),
     labels = names(colPalette), cex = 0.5)

plotDat <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod_dp,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = rev(v_cod_dp), labels = rev(v_cod_lab_dp)),
         Region = factor(Region, levels = v_reg_dths, v_reg_lab)) %>%
  filter(Region != "World") %>%
  rename(env = Deaths) %>%
  mutate(Deaths = CSMF*env) %>%
  mutate(CSMF = CSMF * 100) %>%
  pivot_longer(
    cols = c(Deaths, CSMF),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Variable = ifelse(Variable == "CSMF", "Mortality fractions (%)", "Number of deaths")) %>%
  filter(Year == 2024)
totals <-  plotDat %>%
  filter(Variable == "Number of deaths") %>%
  group_by(Region) %>%
  summarise(Total = sum(Value), .groups = "drop") %>%
  mutate(Variable = "Number of deaths") 
prob_region <- "West and\ncentral Africa"
prob_total <- totals %>% 
  filter(Region == prob_region)
anno_df <- totals %>%
  filter(Region == prob_region) %>%
  mutate(
    y_pos = Total, 
    label = scales::comma(round(Total, 0)),
    Variable = "Number of deaths"
  )
totals <- totals %>%
  filter(Region != "West and\ncentral Africa" )

p_main <- ggplot(plotDat) +
  geom_bar(aes(x = Region, y = Value, fill = COD),
           stat = "identity", position = "stack") +
  geom_text(
    data = totals,
    aes(
      x = Region,
      y = Total,
      label = scales::comma(round(Total, 0))
    ),
    hjust = -0.1, 
    size = 4
  ) +
  geom_segment(
    data = prob_total,
    aes(x = 3.8, xend = 1.6, 
        y = prob_total %>% select(Total) %>% pull() * .98, 
        yend = prob_total %>% select(Total) %>% pull() * .98),
    #arrow = arrow(length = unit(0.15, "cm")),
    linewidth = 0.4
  ) +
  geom_text(
    data = anno_df,
    aes(
      x = 4,
      y = y_pos,
      label = label
    ),
    size = 4,
    hjust = 1
  ) +
  scale_fill_manual(values = colPalette) +
  scale_y_continuous(labels = label_comma()) + #, expand = expansion(mult = c(0, 0.1))) +
  facet_wrap(~Variable, scales = "free_x") +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(t = -10, r = 10, b = 0, l = 10)  
  ) +
  labs(title = "", x = "", y = "") +
  guides(fill = "none")
p_main

legend_order <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod_dp,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  mutate(COD = factor(COD, levels = rev(v_cod_dp), labels = rev(v_cod_lab_dp))) %>%
  select(COD) %>% pull() %>% unique()
causes_1 <- legend_order[1:8]
causes_2 <- legend_order[9:13]
causes_3 <- legend_order[14:20]
make_leg <- function(items, colorlow, colorhigh) {
  df <- data.frame(COD = factor(items, levels = items))
  
  ggplot(df, aes(x = COD, y = 1, fill = COD)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colPalette[colorlow:colorhigh], breaks = items) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = NULL)) +
    theme_void() +
    theme(legend.position = "bottom", text = element_text(size = 13),
          plot.margin = margin(t = -10, r = 10, b = -10, l = 10)  )
}
leg1 <- cowplot::get_legend(make_leg(causes_1, 1, 8))
leg2 <- cowplot::get_legend(make_leg(causes_2, 9, 13))
leg3 <- cowplot::get_legend(make_leg(causes_3, 14, 20))
all_legs <- cowplot::plot_grid(
  leg1,
  leg2,
  leg3,
  ncol = 1,
  rel_heights = c(1,1,1)
)
p_final <- cowplot::plot_grid(
  p_main + guides(fill = "none"),
  all_legs,
  ncol = 1,
  rel_heights = c(1, 0.3)
)
p_final

ggsave(paste("./gen/visualizations/output/csmf_deaths_reg_05to19_2024_v5.png", sep=""), p_final, dpi = 500, height = 6, width = 10, units = "in")



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

# other plots and calculations to assist with interpreting figure 3

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

# old figure 4
# displays all causes

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
         period = ifelse(Year %in% 2000:2015, "2000-2015", "2015-2024"))
# include 2015 in both periods
plotdat2 <- plotdat2 %>%
  bind_rows(plotdat2 %>% filter(Year == 2015) %>% mutate(period = "2015-2024")) %>%
  mutate(period = factor(period, levels = c("2000-2015", "2015-2024"))) %>%
  arrange(Region, COD, Year) %>%
  group_by(Region, COD) %>%
  mutate(lagCSMR = lag(CSMR),
         RR = lagCSMR - CSMR) %>%
  group_by(Region, COD, period) %>%
  summarise(AARR = mean(RR, na.rm = TRUE)) %>%
  mutate(AARRstart = ifelse(period == "2000-2015", AARR, NA),
         AARRend = ifelse(period == "2015-2024", AARR, NA),
         AARRend = max(AARRend, na.rm = TRUE),
         AARRend = ifelse(period == "2015-2024", NA, AARRend)) 


# set cod color palette
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
    labels = c("2000–2015", "2015–2024"),
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
    labels = c("2000–2015", "2015–2024"),
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



# Figure 4: interpretation -------------------------------------------------

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


# Figure 4: aarr high burden ------------------------------------------------------------

# Updated to only display the 6 defined causes (excluding Other..., collective violence and nat disasters) with the highest AARR in MDG or SDG era per region

# # set cod color palette
# mycols <- hue_pal()(length(v_cod))
# cod_colors <- setNames(mycols, v_cod_lab)
# # set cod labels from data portal
# v_cod_lab <- c("Maternal causes", 
#                "Measles", "HIV/AIDS", 
#                "Lower respiratory\ninfections", 
#                "Tuberculosis", "Diarrhoea", "Malaria", 
#                "Other communicable diseases",
#                "Congenital anomalies", "Cardiovascular", "Digestive system", 
#                "Cancer", "Other non-communicable diseases", 
#                "Interpersonal violence", "Self harm",
#                "Drowning", "Road traffic injuries", "Other injuries", 
#                "Natural and unnatural disasters", "Collective violence")


# CODs in data portal order
v_cod_dp <- c("Diarrhoeal", "HIV",  "LRI", "Malaria", 
              "Maternal", "Measles" , "TB", "OtherCMPN", "Cardiovascular",
              "Congenital", "Digestive" ,  "Neoplasms" , "OtherNCD",
              "Drowning" , "RTI",  "SelfHarm" , "InterpVio" ,
              "ColVioNatDis", "OtherInj")
v_cod_lab_dp <- c('Diarrhea', 'HIV/AIDS', 'Lower respiratory\ninfections', 'Malaria',
                  'Maternal causes', 'Measles', 'Tuberculosis', 'Other communicable\ndiseases', 
                  'Cardiovascular', 'Congenital anomalies', 'Digestive system', 
                  'Neoplasms/cancer', 'Other NCDs',
                  'Drowning', 'Road traffic njuries', 'Self-harm', 
                  'Interpersonal violence', 'Collective violence and\nnatural disasters',
                  'Other injuries')

# Color palette 1: Communicable diseases of all ages
col1 <- brewer.pal(9, 'Oranges')[3:9]
col1 <- c(col1[1:4], 'plum1', col1[5:length(col1)])

# Color palette 2: Non-communicable diseases
col2 <- brewer.pal(6, 'Greens')[2:6]

# Color palette 4: Injuries
col3 <- brewer.pal(7, 'Blues')[2:7]

# Full palette
colPalette <- c(col1, col2, col3)
names(colPalette) <- v_cod_lab_dp

# prepare data for aarr calculation
aarrDat <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  filter(!(COD %in% c("NatDis", "CollectVio", "OtherCMPN", "OtherNCD", "OtherInj"))) %>%
  mutate(
    CODfac = factor(COD, levels = v_cod_dp, labels = v_cod_lab_dp),
    Region = factor(Region, levels = v_reg_dths, labels = v_reg_lab_nobreaks)
  ) %>%
  select(-COD) %>%
  rename(COD = CODfac) %>%
  mutate(CSMR = CSMF * Rate,
         period = ifelse(Year %in% 2000:2015, "2000-2015", "2016-2024"))
# include 2015 in both periods
aarrDat <- aarrDat %>%
  bind_rows(aarrDat %>% filter(Year == 2015) %>% mutate(period = "2016-2024")) %>%
  mutate(period = factor(period, levels = c("2000-2015", "2016-2024"))) %>%
  arrange(Region, COD, Year) %>%
  group_by(Region, COD) %>%
  mutate(lagCSMR = lag(CSMR),
         RR = lagCSMR - CSMR) %>%
  group_by(Region, COD, period) %>%
  summarise(AARR = mean(RR, na.rm = TRUE)) %>%
  mutate(AARR1 = ifelse(period == "2000-2015", AARR, NA),
         AARR2 = ifelse(period == "2016-2024", AARR, NA),
         AARR2 = max(AARR2, na.rm = TRUE),
         AARR2 = ifelse(period == "2016-2024", NA, AARR2)) 
# only keep top 6 aarr by cause, by region
aarrDat <- aarrDat %>%
  group_by(Region) %>%
  arrange(Region, -AARR) %>%
  mutate(rank = 1:n()) %>%
  group_by(Region, COD) %>%
  mutate(minRank = min(rank)) %>%
  arrange(Region, minRank) %>%
  group_by(Region) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 12)

# prepare cause-specific rates data
csmrDat <- aggReg5to19 %>%
  pivot_longer(
    cols = v_cod,
    names_to = "COD",
    values_to = "CSMF"
  ) %>%
  filter(!(COD %in% c("NatDis", "CollectVio", "OtherCMPN", "OtherNCD", "OtherInj"))) %>%
  mutate(
    COD = factor(COD, levels = v_cod_dp, labels = v_cod_lab_dp),
    Region = factor(Region, levels = v_reg_dths, labels = v_reg_lab_nobreaks)
  ) %>%
  mutate(CSMR = CSMF * Rate) %>%
  inner_join(aarrDat %>% select(Region, COD) %>% distinct(), by = c("Region", "COD"))

# define function for csmr plot
fn_plotCSMR <- function(DAT, REGION, YLIMIT, XLAB){
  
  v_cod_order <- DAT %>%
    filter(Region == REGION) %>%
    filter(Year == 2024) %>%
    arrange(CSMR) %>%
    select(COD) %>% pull()
  
  ybreaks <- c(0, floor(YLIMIT/4) * seq(1,4,1), YLIMIT)
  ybreaks <- unique(ybreaks)
  if(length(ybreaks) <= 2){
    ybreaks <- c(0, round(YLIMIT/2), YLIMIT)
  }
  
  plot <- DAT %>%
    filter(Region == REGION) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = 2015, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
    annotate("rect", xmin = 2015, xmax = Inf, ymin = -Inf, ymax = Inf,fill = "white", alpha = 1) +
    geom_line(aes(x = Year, y = CSMR, color = COD)) +
    geom_vline(aes(xintercept = 2015)) +
    labs(x = XLAB, y = "") +
    scale_color_manual(values = colPalette) +
    scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020, 2024)) +
    scale_y_continuous(breaks = ybreaks) +
    theme(legend.position = "none", plot.margin = margin(0, 0, 0, 1)) +
    coord_cartesian(ylim = c(0, YLIMIT))
  
  return(plot)
  
}

# define function for AARR plot
fn_plotAARR <- function(DAT, REGION, XLOW, XHIGH, XLAB){
  
  v_cod_order <- DAT %>%
    filter(Region == REGION) %>%
    filter(Year == 2000) %>%
    arrange(CSMR) %>%
    select(COD) %>% pull()
  
  if(XHIGH <= .1){
    # XLOW <- -0.02
    # XHIGH <- 0.07
    xbreaks <- c(XLOW, 0, XHIGH * c(0.3, 0.5, 0.8))
    xbreaks <- round(xbreaks, 2)
    xbreaks
  }else{
    #XLOW <- -0.01
    #XHIGH <- 0.46
    xbreaks <- c(0, round(XHIGH/4 * seq(1,4,1),1), floor(XHIGH * 10) / 10)
    xbreaks <- unique(xbreaks)
    xbreaks <- xbreaks[xbreaks <= XHIGH]
    xbreaks
  }
  XLOW <- XLOW * 100 
  XHIGH <- XHIGH * 100
  xbreaks <- xbreaks * 100
  
  plot <- aarrDat %>%
    filter(Region == REGION) %>%
    mutate(COD = factor(COD, levels = v_cod_order)) %>%
    ggplot() +
    geom_point(aes(x = COD, y = AARR*100, shape = period, color = COD)) +
    geom_segment(aes(x = COD, xend = COD, y = AARR1*100, yend = AARR2*100, color = COD), 
                 arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
                 show.legend = FALSE) +
    geom_hline(aes(yintercept = 0)) +
    labs(x = "", y = XLAB) +
    scale_shape_manual(values = c(16, NA)) +
    scale_color_manual(values = colPalette) +
    scale_y_continuous(limits = c(XLOW, XHIGH), breaks = xbreaks) +
    guides(shape  = "none", color = "none") +
    theme(plot.margin = margin(0, 0, 0, 0)) +
    coord_flip()
  
  return(plot)
}

# create each region plot
p1a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[2], YLIMIT = 10, XLAB = "")
p1b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[2], XLOW = -0.01, XHIGH = 0.45, XLAB = "")
p1 <- p1a + p1b + plot_annotation(subtitle = v_reg_lab_nobreaks[2])

p2a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[3], YLIMIT = 10, XLAB = "")
p2b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[3], XLOW = -0.01, XHIGH = 0.45, XLAB = "")
p2 <- p2a + p2b + plot_annotation(subtitle = v_reg_lab_nobreaks[3])

p3a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[4], YLIMIT = 5, XLAB = "")
p3b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[4], XLOW = -0.01, XHIGH = 0.45, XLAB = "")
p3 <- p3a + p3b + plot_annotation(subtitle = v_reg_lab_nobreaks[4])

p4a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[5], YLIMIT = 5, XLAB = "Year")
p4b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[5], XLOW = -0.01, XHIGH = 0.45, XLAB = "Average annual rate of reduction (%)")
p4 <- p4a + p4b + plot_annotation(subtitle = v_reg_lab_nobreaks[5])

# add legend to bottom of lowest plot
dummy_plot <- ggplot() +
  dummy_guide(
    labels = c("2000–15", "2016–24"),
    shape  = c(16, 17),
    title = "Period"
  ) +
  theme_void() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.direction = "horizontal",
        plot.margin = margin(0, 0, 0, 0))

p4 <- p4  / dummy_plot + plot_layout(heights = c(1, 0.1)) + plot_annotation(subtitle = v_reg_dths[5])

# combine all plots together
aarr1 <- (wrap_elements(p1) / wrap_elements(p2) / wrap_elements(p3) /  wrap_elements(p4)) +
  plot_layout(heights = c(1, 1, 1, 1.2)) &
  theme(plot.margin = margin(-1, -1, -1, 10))

# add a y-axis label to the left side
add_y_label <- ggplot() +
  theme_void() +
  annotate("text", x = 0.5, y = 0.5, label = "Deaths per 1000 population",
           angle = 90, size = 4.25, hjust = 0.5) # WAS 3.5
aarr1_labeled <- add_y_label + aarr1 + plot_layout(widths = c(0.05, 1))

aarr1_labeled 

# save
ggsave( "./gen/visualizations/output/aar1-abbrev.png", aarr1_labeled, width = 8, height = 10, dpi = 500)


# Appendix Figure: aarr low burden ----------------------------------------

# create each region plot
p1a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[6], YLIMIT = 2, XLAB = "")
p1b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[6], XLOW = -0.02, XHIGH = 0.07, XLAB = "")
p1 <- p1a + p1b + plot_annotation(subtitle = v_reg_lab_nobreaks[6])

p2a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[7], YLIMIT = 2, XLAB = "")
p2b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[7], XLOW = -0.02, XHIGH = 0.07, XLAB = "")
p2 <- p2a + p2b + plot_annotation(subtitle = v_reg_lab_nobreaks[7])

p3a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[8], YLIMIT = 2, XLAB = "")
p3b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[8], XLOW = -0.02, XHIGH = 0.07, XLAB = "")
p3 <- p3a + p3b + plot_annotation(subtitle = v_reg_lab_nobreaks[8])

p4a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[9], YLIMIT = 2, XLAB = "")
p4b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[9], XLOW = -0.02, XHIGH = 0.07, XLAB = "")
p4 <- p4a + p4b + plot_annotation(subtitle = v_reg_lab_nobreaks[9])

p5a <- fn_plotCSMR(csmrDat, v_reg_lab_nobreaks[10], YLIMIT = 2, XLAB = "Year")
p5b <- fn_plotAARR(csmrDat, v_reg_lab_nobreaks[10], XLOW = -0.02, XHIGH = 0.07, XLAB = "Average annual rate of reduction (%)")
p5 <- p5a + p5b + plot_annotation(subtitle = v_reg_lab_nobreaks[10])

# add legend to bottom of lowest plot
dummy_plot <- ggplot() +
  dummy_guide(
    labels = c("2000–2015", "2016–2024"),
    shape  = c(16, 17),
    title = "Period"
  ) +
  theme_void() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.direction = "horizontal",
        plot.margin = margin(0, 0, 0, 0))

p5 <- p5  / dummy_plot + plot_layout(heights = c(1, 0.1)) + plot_annotation(subtitle = v_reg_dths[10])

# combine all plots together
aarr2 <- (wrap_elements(p1) / wrap_elements(p2) / wrap_elements(p3) /  wrap_elements(p4) /  wrap_elements(p5)) +
  plot_layout(heights = c(1, 1, 1, 1, 1.18)) &
  theme(plot.margin = margin(-1, -1, -1, 10))

# add a y-axis label to the left side
add_y_label <- ggplot() +
  theme_void() +
  annotate("text", x = 0.5, y = 0.5, label = "Deaths per 1,000 population",
           angle = 90, size = 4.25, hjust = 0.5)
aarr2_labeled <- add_y_label + aarr2 + plot_layout(widths = c(0.05, 1))

aarr2_labeled 

# save
ggsave( "./gen/visualizations/output/aar2-abbrev.png", aarr2_labeled, width = 8, height = 10, dpi = 500)


