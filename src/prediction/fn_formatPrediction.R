fn_formatPrediction <- function(CSMF_HMM = NULL, CSMF_LMM = NULL){
  
  #' @title Format predicted CSMFs
  # 
  #' @description Combines predicted CSMFs for HMM and LMM countries.
  #
  #' @param CSMF_HMM Dataframe with predicted CSMFs for HMM.
  #' @param CSMF_LMM Dataframe with predicted CSMFs for HMM.
  #' @return Dataframe with predicted CSMFs for HMM and LMM.
  
  if(!is.null(CSMF_HMM) & !is.null(CSMF_LMM)){
    dat <- rbind(CSMF_HMM, CSMF_LMM)
  }
  if(!is.null(CSMF_HMM) & is.null(CSMF_LMM)){
    dat <- CSMF_HMM
  }
  if(is.null(CSMF_HMM) & !is.null(CSMF_LMM)){
    dat <- CSMF_LMM
  }
  
  # Rearrange columns
  dat <- dat[, c(idVars[1:2], sort(names(dat)[which(!names(dat) %in% idVars[1:2])]))] 
  
  # Tidy up
  dat <- dat[order(dat$iso3, dat$year),]
  
  return(dat)
  
}
