fn_reshapePr2 <- function(DAT, UNCERTAINTY = FALSE){
  
  #' @title Reshape output from fn_pr2
  # 
  #' @description Reshapes output wide and only keeps median prediction. This needs to be updated for estimating uncertainty
  #
  #' @param DAT Output from fn_pci2
  #' @return Data frame with predicted CSMFs for each country and year
  
  # # testing
  # DAT <- mod_pred_HMM
  # UNCERTAINTY <- FALSE
  
  # Point estimates from fit
  csmf <- DAT$Point_estimates %>%
    pivot_wider(
      id_cols = c(iso3, year),
      names_from = cod,
      values_from = pe.q2
    )
  
  # # All predictions from fit (prediction with just fixed effects (pf) and fixed plus random (pr))
  # # Use this for uncertainty?
  # if(UNCERTAINTY){
    # DAT$Predictions %>% 
    #   pivot_longer(cols=c(pf:pr), names_to="model", values_to="value") %>%
    #   arrange(iso3, year, sample, cod, model)
  # }

  
  return(csmf)
}
