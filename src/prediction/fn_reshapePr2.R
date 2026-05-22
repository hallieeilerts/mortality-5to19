fn_reshapePr2 <- function(DAT, UNCERTAINTY = FALSE){
  
  #' @title Reshape output from fn_pr2
  # 
  #' @description Reshapes output wide and only keeps median prediction. This needs to be updated for estimating uncertainty
  #
  #' @param DAT Output from fn_pr2
  #' @return Data frame with predicted CSMFs for each country and year
  
  # Point estimate predictions (median of coefficients)
  if(!UNCERTAINTY){
    csmf <- DAT$Point_estimates %>%
      pivot_wider(
        id_cols = c(iso3, year),
        names_from = cod,
        values_from = pe.q2
      )
  }
  
  # Uncertainty predictions (point estimates from each draw)
  if(UNCERTAINTY){
    csmf <- DAT$Predictions %>%
      pivot_wider(
        id_cols = c(sample, iso3, year),
        names_from = cod,
        values_from = pr
      )
    csmf <- split(csmf, csmf$sample)
    csmf <- lapply(csmf, function(x){ row.names(x) <- NULL; return(x) })
    csmf <- lapply(csmf, function(x){ x$sample <- NULL; return(x) })
    
  }

  return(csmf)
  
}
