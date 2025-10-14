fn_formatAllOutput <- function(CSMF_ALL, KEY_CODLIST){
  
  #' @title Combine CSMFs that have been squeezed with GOODVR countries
  # 
  #' @description Combine data frames, rename crisis-included deaths and rates columns.
  #
  #' @param CSMF_ALL Data frame with CSMFs that have been processed by squeezing functions and CSMFs for GOODVR countries that have not been squeezed.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with CSMFs that have been processed by squeezing functions, CSMFs that were not squeezed (GOODVR), all-cause crisis-free and crisis-included deaths and rates.
  
  dat <- CSMF_ALL
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
  #v_cod <- unique(subset(KEY_CODLIST, !is.na(cod_reported))$cod_reported)   
  
  # Arrange columns
  v_cols <- c(idVars, "Deaths1", "Rate1", "Deaths2", "Rate2", v_cod)
  v_cols <- v_cols[v_cols %in% names(dat)]
  dat <- dat[, paste(v_cols)]
  
  # Tidy up
  dat <- dat[order(dat$iso3, dat$year), ]
  rownames(dat) <- NULL
  
  return(dat)
  
}