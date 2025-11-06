fn_createSampleVectors <- function(L_CSMFDRAWS, L_ENVDRAWS){
  
  #' @title Create randomly sampled vectors for igme draws and predicted fractions
  # 
  #' @description Randomly samples draws from envelopes and predicted LMM and HMM fractions.
  #' Saves sample vectors in a list.
  #
  #' @param L_CSMFDRAWS List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted
  #' @param L_ENVDRAWS List of length three, corresponding to crisis-free deaths, crisis-included deaths, crisis-included rates.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is a data frame 
  #' with a draw with columns c('ISO3', 'Year', 'Sex', 'Deaths1', 'Deaths2', 'Rate').
  #' @return List of length two, corresponding to envelope and CSMF draws
  #' Each list element is a vector (all of equal length) with a list of integers.
  #' Integers are randomly sampled draws from envelope and CSMFs.
  
  v_sample_env <- sort(sample(x = length(L_ENVDRAWS$deaths1), size = length(L_CSMFDRAWS))) 
  
  v_sample_csmf <- 1:length(L_CSMFDRAWS)
  
  # Combine all sample vectors into list
  l_sample <- list(v_sample_env, v_sample_csmf)
  names(l_sample) <- c("env", "csmf")
  
  return(l_sample)
}
