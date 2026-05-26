fn_checkCSMFsqz <- function(CSMF, KEY_CODLIST){
  
  #' @title Check if squeezed CSMFs add up to 1 or contain NAs
  # 
  #' @description Checks for country-years where squeezed fractions do not add up to 1.
  #
  #' @param CSMF Data frame with CSMFs that have been processed by squeezing functions, all-cause crisis-free and crisis-included deaths and rates.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with rows where fractions for country-year do not add up to 1 or contain an NA.
  
  dat <- CSMF
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
  
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

  dat <- dat[c(v_audit),]
  dat$csmf_SUM <- round(rowSums(dat[, paste(v_cod)]),5)
  rownames(dat) <- NULL
  
  return(dat)
}
