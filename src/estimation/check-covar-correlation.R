################################################################################
#' @description Check covariate correlation. Part of process for selecting covariates for each model.
#' @return Correlation plots.
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
source("./src/estimation/fn_setRefCat.R")
model <- "HMM"
if(model == "HMM"){
  ## Model input data frames
  dat_filename <- list.files("./data/model-objects/")
  dat_filename <- dat_filename[grepl("modinput2023-hmm-deaths", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  load(paste0("./data/model-objects/",dat_filename, sep = ""))
  # dat_filename <- list.files("./data/study-database/")
  # dat_filename <- dat_filename[grepl("modinput2023-studies", dat_filename, ignore.case = TRUE)]
  # dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  # load(paste0("./data/study-database/",dat_filename, sep = ""))
  # Load in entire study db spreadsheet instead since not decided on covariates
  dat_filename <- list.files("./data/study-database/")
  dat_filename <- dat_filename[grepl("studydatabase2023_", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  dat_filename <- dat_filename[!grepl("Codebook", dat_filename)] 
  studies <- read.csv(paste0("./data/study-database/", dat_filename))
}
if(model == "LMM"){
  ## Model inputs for LMM
  dat_filename <- list.files("./data/model-objects/")
  dat_filename <- dat_filename[grepl("modinput2023-lmm-deaths", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  load(paste0("./data/model-objects/",dat_filename, sep = ""))
  # dat_filename <- list.files("./data/study-database/")
  # dat_filename <- dat_filename[grepl("modinput2023-lmm-studies", dat_filename, ignore.case = TRUE)]
  # dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  # load(paste0("./data/study-database/",dat_filename, sep = ""))
  # Load in entire lmm spreadsheet instead since not decided on covariates
  dat_filename <- list.files("./data/lmm-database/")
  dat_filename <- dat_filename[grepl("studydatabase2023-lmm_", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  dat_filename <- dat_filename[!grepl("Codebook", dat_filename)] 
  studies <- read.csv(paste0("./data/lmm-database/", dat_filename))
  
}
## Classification keys
#key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageSexSuffix, ".csv", sep=""))
#key_codlist <- read.csv(paste("./gen/data-management/output/key_codlist_", ageSexSuffix, ".csv", sep=""))
df_covar <- readxl::read_excel("./data/classification-keys/CovariateDatabase2023_ModelCovariateList_20250618.xlsx", sheet = "model-covar-long")
#############################################################################################

if(length(unique(deaths$sid)) != length(unique(studies$sid))){
  warning('studies and deaths should contain the same number of studies.')
}

# Create model object that exactly matches object from last time
if(model == "HMM"){
  refCat <- fn_setRefCat(ageSexSuffix, CTRYGRP = "HMM")
  #vxf <- fn_setCov(ageSexSuffix, CTRYGRP = "HMM")
  #vdt <- subset(key_codlist, ModeledOrReported == "HMM")$COD
}
if(model == "LMM"){
  refCat <- fn_setRefCat(ageSexSuffix, CTRYGRP = "LMM")
  #vxf <- fn_setCov(ageSexSuffix, CTRYGRP = "LMM")
  #vdt <- subset(key_codlist, ModeledOrReported == "LMM")$COD
}


#vdt <- as.factor(c(refCat, paste(vdt[vdt != refCat])))
sex <- sexLabel

# to create model input studies
# v_idvars <- c("recnr","id", "reterm", "totdeaths")
# df_covar <- readxl::read_excel("./data/classification-keys/CovariateDatabase2023_ModelCovariateList_20250430.xlsx", sheet = "model-covar-long")
vxf <- subset(df_covar, Model == model & AgeSexSuffix == ageSexSuffix)$Covariate
# studies <- dat[, c(v_idvars, vxf)]

# # add to all
# vxf <- unique(vxf)
# if(ageSexSuffix %in% c("05to09y","10to14y")){
#   vxf <- c(vxf, "unemployment_mf")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix %in% c("15to19yF")){
#   vxf <- c(vxf, "unemployment_f", "unemployment_neet_f", "edu_mean_f")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix %in% c("15to19yM")){
#   vxf <- c(vxf, "unemployment_m", "unemployment_neet_m", "edu_mean_m")
#   vxf <- unique(vxf)
# }
# 
# 
# if(ageSexSuffix == "05to09y" & model == "HMM"){
#   # changes when using fn_setCov
#   # vxf <- vxf[!(vxf == "year")]
#   # vxf <- vxf[!(vxf == "mr05to19")]
#   # vxf <- c(vxf, "mr05to14")
#   # changes when using CovariateDatabase2023_ModelCovariateList_20250514.xlsx
#   vxf <- vxf[!(vxf %in% c("edu_mean_mf", "unemployment_neet_mf"))]
#   vxf <- c(vxf, "literacy_mf", "literacy_f", "ors_mf",  "u5pop_mf", "underweight")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix == "05to09y" & model == "LMM"){
#   # changes when using fn_setCov
#   # vxf <- vxf[!(vxf == "year")]
#   # vxf <- vxf[!(vxf == "mr05to19")]
#   # vxf <- c(vxf, "mr05to14")
#   # changes when using CovariateDatabase2023_ModelCovariateList_20250514.xlsx
#   vxf <- vxf[!(vxf %in% c("gini", "height_mf", "unemployment_neet_mf"))]
#   vxf <- c(vxf, "literacy_mf", "literacy_f", "obese_mf", "u5pop_mf", "underweight")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix == "10to14y" & model == "HMM"){
#   # changes when using fn_setCov
#   # vxf <- vxf[!(vxf == "year")]
#   # vxf <- vxf[!(vxf == "mr05to19")]
#   # vxf <- c(vxf, "mr05to14")
#   # changes when using CovariateDatabase2023_ModelCovariateList_20250514.xlsx
#   vxf <- vxf[!(vxf %in% c("unemployment_neet_mf"))]
#   vxf <- c(vxf,  "height_mf", "literacy_mf", "literacy_f", "sex_age15_f", "underweight")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix == "10to14y" & model == "LMM"){
#   # changes when using fn_setCov
#   # vxf <- vxf[!(vxf == "year")]
#   # vxf <- vxf[!(vxf == "mr05to19")]
#   # vxf <- c(vxf, "mr05to14")
#   # changes when using CovariateDatabase2023_ModelCovariateList_20250514.xlsx
#   vxf <- vxf[!(vxf %in% c("gini", "ors_mf"))]
#   vxf <- c(vxf, "edu_mean_mf", "literacy_mf", "literacy_f", "obese_mf", "sex_age15_f", "underweight")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix == "15to19yF" & model == "HMM"){
#   # changes when using fn_setCov
#   # vxf <- vxf[!(vxf == "year")]
#   # vxf <- vxf[!(vxf == "mr05to19")]
#   # vxf <- c(vxf, "mr15to19_mf")
#   # changes when using CovariateDatabase2023_ModelCovariateList_20250514.xlsx
#   vxf <- c(vxf, "labor_participation_f", "marriage_f", "thinness_f")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix == "15to19yF" & model == "LMM"){
#   # changes when using fn_setCov
#   # vxf <- vxf[!(vxf == "year")]
#   # vxf <- vxf[!(vxf == "mr05to19")]
#   # vxf <- c(vxf, "mr15to19_mf")
#   # changes when using CovariateDatabase2023_ModelCovariateList_20250514.xlsx
#   vxf <- c(vxf, "depression_f", "labor_participation_f", "sex_age15_f")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix == "15to19yM" & model == "HMM"){
#   # changes when using fn_setCov
#   #vxf <- vxf[!(vxf == "year")]
#   #vxf <- vxf[!(vxf == "mr05to19")]
#   #vxf <- c(vxf, "mr15to19_mf")
#   # changes when using CovariateDatabase2023_ModelCovariateList_20250514.xlsx
#   vxf <- c(vxf, "sex_age15_m", "thinness_m")
#   vxf <- unique(vxf)
# }
# if(ageSexSuffix == "15to19yM" & model == "LMM"){
#   # changes when using fn_setCov
#   #vxf <- vxf[!(vxf == "year")]
#   #vxf <- vxf[!(vxf == "mr05to19")]
#   #vxf <- c(vxf, "mr15to19_mf")
#   # changes when using CovariateDatabase2023_ModelCovariateList_20250514.xlsx
#   vxf <- c(vxf, "labor_participation_m")
#   vxf <- unique(vxf)
# }

# Discard corruption and birth rate
# changes when using fn_setCov
#vxf <- vxf[!(vxf %in% c("birthrate", "corruption"))]


# create correlation matrix

# if some non-numeric
# cor_matrix <- your_data %>%
#   select(where(is.numeric)) %>%
#   cor(use = "pairwise.complete.obs")

# for(i in 1:length(vxf)){
#   print(vxf[i])
#   print(vxf[i] %in% names(studies))
# }

# look at high correlation in 5-9y hmm thinness and u5pop
# library(tidyr)
# library(dplyr)
# library(ggplot2)
# label_points <- studies %>%
#   filter(iso3 %in% c("IND", "MDG")) %>%
#   group_by(iso3) %>%
#   slice(5) %>%   # pick the first occurrence; change to slice_max/min if needed
#   ungroup() %>%
#   mutate(
#     text_x = thinness_mf + .05,  # shift text to the right
#     text_y = u5pop_mf + .05      # shift text upward
#   )
# ggplot(studies, aes(x = thinness_mf, y = u5pop_mf)) +
#   geom_point(aes(color = iso3)) +
#   geom_smooth(method = "lm") +
#   geom_text(
#     data = label_points,
#     aes(x = text_x, y = text_y, label = iso3),
#     color = "black",
#     fontface = "bold"
#   ) +
#   geom_segment(
#     data = label_points,
#     aes(x = text_x, y = text_y, xend = thinness_mf, yend = u5pop_mf),
#     arrow = arrow(length = unit(0.02, "npc")),
#     color = "black"
#   )


numeric_data <- studies[,vxf][sapply(studies[,vxf], is.numeric)]
numeric_data <- numeric_data[, order(colnames(numeric_data))]
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
reg <- function(x, y){
  points(x,y, col="lightblue")
  abline(lm(y~x), col="blue")
  abline(v=0, h=0, col="red", lty=3)
}  

# correlation coefficients all the same size
# panel.cor <- function(x, y){
#   par(usr = c(0, 1, 0, 1))
#   r <- cor(x, y, use="complete.obs")
#   text(0.5, 0.5, formatC(r, 2, format="f"), cex=2)
# }
# larger text for larger coefficients
panel.cor <- function(x, y, digits = 2, min_cex = 1, max_cex = 4) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y, use = "complete.obs")
  txt <- formatC(r, digits, format = "f")
  
  # Scale cex based on |r|
  cex <- min_cex + (max_cex - min_cex) * abs(r)
  
  text(0.5, 0.5, txt, cex = cex)
}

# original plot
# png(paste0("./gen/estimation/audit/corr-plots-20250605/corr-", model, "-", ageSexSuffix,"_", format(Sys.Date(), "%Y%m%d"), ".png"), width = 1200, height = 1200)
# pairs(studies[, vxf],
#       lower.panel = reg, upper.panel = panel.cor,
#       gap = 0, row1attop = FALSE, cex.labels = 1.5)
# dev.off()

# Custom panel for diagonal with wrapped variable names
panel.diag.wrap <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  var_name <- deparse(substitute(x))  # Will be overwritten, workaround below
}
# Wrapping labels in column names
wrapped_names <- sapply(colnames(numeric_data), function(name) {
  paste(strwrap(name, width = 10), collapse = "\n")
})
colnames(numeric_data) <- wrapped_names
# Plot with wrapping
png(paste0("./gen/estimation/audit/corr-plots-20250618/corr-", model, "-", ageSexSuffix,"_", format(Sys.Date(), "%Y%m%d"), ".png"), width = 1200, height = 1200)
pairs(numeric_data,
      lower.panel = reg,
      upper.panel = panel.cor,
      diag.panel = function(x, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        i <- which(sapply(numeric_data, identical, x))  # get index
        text(0.5, 0.5, wrapped_names[i], cex = 1.2)
      }
)
dev.off()

