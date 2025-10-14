fn_checkCSMFsqz <- function(CSMF, KEY_CODLIST){
  
  #' @title Check if squeezed CSMFs add up to 1 or contain NAs
  # 
  #' @description Checks for country-years where squeezed fractions do not add up to 1.
  #
  #' @param CSMF Data frame with CSMFs that have been processed by squeezing functions, all-cause crisis-free and crisis-included deaths and rates.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with rows where fractions for country-year do not add up to 1 or contain an NA.
  
  # testing
  CSMF <- csmf_SQZ
  KEY_CODLIST <- key_codlist
  
  dat <- CSMF
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
  #v_cod <- unique(subset(KEY_CODLIST, !is.na(cod_reported))$cod_reported)   
  
  v_containsNA <- which(is.na(rowSums(dat[, paste(v_cod)])))
  v_sumnot1    <- which(round(rowSums(dat[, paste(v_cod)]),5) != 1)
  v_audit      <- c(v_containsNA, v_sumnot1)
  v_audit      <- unique(v_audit)
  v_audit      <- sort(v_audit)
  
  # Checks
  if(any(is.na(rowSums(dat[, paste(v_cod)])))){
    warning("CSMFs contain NA")
  }
  if(any(round(rowSums(dat[, paste(v_cod)]),5) != 1)){
    warning("CSMFs do not add up to 1")
  }
  #print(round(rowSums(DAT[, paste(v_cod)]),7), digits = 20)
  #table(rowSums(DAT[, paste(v_cod)]))
  #table(round(rowSums(DAT[, paste(v_cod)]),5))
  #DAT[which(round(rowSums(DAT[, paste(v_cod)]),5) == 0.99942),]
  #DAT[which(rowSums(DAT[, paste(v_cod)]) != 1),]
  #foo <- DAT[which(rowSums(DAT[, paste(v_cod)]) > 1.18),]
  #foo[, c("ISO3",paste(v_cod))]
  
  # Check rows that don't add to 0
  # v_check <- which(round(rowSums(dat[, paste(v_cod)]),5) != 1)
  # df_check <- dat[v_check,]
  # df_check$total <-  round(rowSums(df_check[, paste(v_cod)]),5)
  # View(df_check)
  
  dat <- dat[c(v_audit),]
  dat$csmf_SUM <- round(rowSums(dat[, paste(v_cod)]),5)
  rownames(dat) <- NULL
  
  return(dat)
}

# # Finding that some fractions don't sum to 1. Trying to see where this happens in the squeezing process.
# 
# # To begin with, some don't sum to 1
# # This is probably due to the prediction functions
# # they are all be close to 1 though
# CSMF <- csmf
# KEY_CODLIST <- key_codlist
# dat <- CSMF
# v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
# v_codpres <- v_cod[v_cod %in% names(dat)]
# v_codpres
# dat$total <- rowSums(dat[, paste(v_codpres)])
# which(round(rowSums(dat[, paste(v_codpres)]),5) != 1)
# v_not1 <- which(round(rowSums(dat[, paste(v_codpres)]),5) != 1)
# dat[v_not1,]
# v_farfrom1 <- which(round(rowSums(dat[, paste(v_codpres)]),2) != 1)
# dat[v_farfrom1,]
# v_reallyfarfrom1 <- which(round(rowSums(dat[, paste(v_codpres)]),2) < 0.99)
# dat[v_reallyfarfrom1,]
# 
# # "Congenital" "Diarrhoeal" "Digestive"  "Drowning"   "LRI"        "Malaria"    "Neoplasms"  "RTI"        "OtherCMPN"  "OtherNCD"   "OtherInj" 
# 
# # no change
# CSMF <- csmf_othercmpnSQZ
# KEY_CODLIST <- key_codlist
# dat <- CSMF
# v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
# v_codpres <- v_cod[v_cod %in% names(dat)]
# v_codpres
# dat$total <- rowSums(dat[, paste(v_codpres)])
# rowSums(dat[, paste(v_codpres)])
# which(round(rowSums(dat[, paste(v_codpres)]),5) != 1)
# v_not1 <- which(round(rowSums(dat[, paste(v_codpres)]),5) != 1)
# dat[v_not1,]
# v_reallyfarfrom1 <- which(round(rowSums(dat[, paste(v_codpres)]),2) < 0.99)
# dat[v_reallyfarfrom1,]
# # [1] "Congenital" "Diarrhoeal" "Digestive"  "Drowning"   "LRI"        "Malaria"    "Neoplasms"  "RTI"        "OtherCMPN"  "OtherNCD"   "OtherInj"   
# # "Measles"   "HIV"        "TB"   
# 
# # no change
# CSMF <- csmf_lriSQZ
# KEY_CODLIST <- key_codlist
# dat <- CSMF
# v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
# v_codpres <- v_cod[v_cod %in% names(dat)]
# v_codpres
# dat$total <- rowSums(dat[, paste(v_codpres)])
# rowSums(dat[, paste(v_codpres)])
# which(round(rowSums(dat[, paste(v_codpres)]),5) != 1)
# v_not1 <- which(round(rowSums(dat[, paste(v_codpres)]),5) != 1)
# dat[v_not1,]
# v_reallyfarfrom1 <- which(round(rowSums(dat[, paste(v_codpres)]),2) < 0.99)
# dat[v_reallyfarfrom1,]
# #  [1] "Congenital" "Diarrhoeal" "Digestive"  "Drowning"   "LRI"        "Malaria"    "Neoplasms"  "RTI"        "OtherCMPN"  "OtherNCD"   "OtherInj"  
# # "Measles"    "HIV"        "TB"     
# 
# # HERE is where the problem starts
# CSMF <- csmf_crisisEndSQZ
# KEY_CODLIST <- key_codlist
# dat <- CSMF
# v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
# v_codpres <- v_cod[v_cod %in% names(dat)]
# v_codpres
# dat$total <- rowSums(dat[, paste(v_codpres)])
# rowSums(dat[, paste(v_codpres)])
# which(round(rowSums(dat[, paste(v_codpres)]),5) != 1)
# v_not1 <- which(round(rowSums(dat[, paste(v_codpres)]),5) != 1)
# dat[v_not1,]
# v_reallyfarfrom1 <- which(round(rowSums(dat[, paste(v_codpres)]),2) < 0.99)
# dat[v_reallyfarfrom1,]
# # [1] "Congenital" "Diarrhoeal" "Digestive"  "Drowning"   "LRI"        "Malaria"    "Neoplasms"  "RTI"  "OtherCMPN"  "OtherNCD"   "OtherInj"  
# # "CollectVio" "NatDis"    "Measles"    "HIV"        "TB" 
