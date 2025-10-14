fn_loadModFit <- function(AGESEXSUFFIX, MODEL, DAT_HP, LAM = NULL, RESD = NULL){
  
  #' @title Load model data
  # 
  #' @description Loads studies and deaths for LMM or HMM
  #
  #' @param AGESEXSUFFIX ageSexSuffix as set in prepare-session
  #' @param MODEL "HMM" or "LMM"
  #' @param DAT_HP data frame with hyperparameters
  #' @param LAM manual specification of lamba instead of the one saved in dat_hp (optional)
  #' @param RESD manual specification of resd instead of the one saved in dat_hp (optional)
  #' @return List with data frames for studies and deaths
  
  
  resd <- subset(DAT_HP, Model == MODEL & AgeSexSuffix == AGESEXSUFFIX)$sdre
  lam <- subset(DAT_HP, Model == MODEL & AgeSexSuffix == AGESEXSUFFIX)$lamb
  
  if(!is.null(RESD)){
    resd <- RESD
  }
  
  if(!is.null(LAM)){
    lam <- LAM
  }

  fileName <- paste('Stan', MODEL, AGESEXSUFFIX, resd, lam, sep = '_')
  load(paste0("./gen/estimation/output/", fileName, '.RData'))
  dat <- get(fileName)
  
  return(dat)

}
