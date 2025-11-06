fn_roundCSMFsqz <- function(CSMFSQZ, KEY_CODLIST = NULL, AGGAGE = FALSE, CODALL = NULL){
  
  #' @title Round squeezed CSMFs
  # 
  #' @description Rounds all-cause deaths/rates and squeezed CSMFs. This is done in case we want to share our CSMF estimates prior to being ready to run the uncertainty pipeline. The uncertainty pipeline will round the point estimates (see fn_roundPointInt()) and do minor adjustments some of the point estimates (see fn_adjust_pointint()). However the uncertainty pipeline may not be ready to be run due to missing inputs.
  #
  #' @param CSMFSQZ Data frame with CSMFs that have been processed in squeezing pipeline (contains all countries, even those not subject to squeezing).
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @return Data frame with all-cause deaths/rates and squeezed CSMFs rounded to the same number of digits as the function fn_roundPointInt() in the uncertainty pipeline.
  
  dat <- data.frame(CSMFSQZ)
  
  # COD vector
  if(!AGGAGE){
    # Vector with causes of death for 5 year age groups
    v_cod <- unique(subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD)
  }
  if(AGGAGE){
    # Vector with all causes of death
    v_cod <- CODALL[CODALL %in% names(dat)]
  }
  
  # Round all-cause deaths
  dat$Deaths2 <- round(dat$Deaths2)
  
  # Round all-cause rate
  dat$Rate2 <- round(dat$Rate2, 5)
  
  # Round cause-specific fractions
  dat[,v_cod] <- round(dat[,v_cod], 5)

  # Tidy up
  rownames(dat) <- NULL
  return(dat)
  
}
