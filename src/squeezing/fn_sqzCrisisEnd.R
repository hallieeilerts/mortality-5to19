
fn_sqzCrisisEnd <- function(CSMF, KEY_CODLIST, UNCERTAINTY = FALSE){
  
  #' @title Squeeze endemic crisis deaths
  # 
  #' @description Add endemic crisis single cause deaths to all-cause crisis-free deaths. Calculate fractions for endemic crisis single causes from this sum. Subtract endemic crisis fractions from 1, squeeze other fractions into remaining space.
  #' 
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param UNCERTAINTY A boolean that denotes whether this function is being run as part of the squeezing pipeline or uncertainty pipeline.
  #' @return Data frame where CSMFs have been adjusted for endemic crisis squeezing.
  
  dat <- CSMF
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
  
  # other communicable diseases in this age group
  v_allcd <- c("OtherCMPN", "LRI", "Diarrhoeal", "TB")
  v_allcd <- v_allcd[v_allcd %in% v_cod]
  
  # Add crisis-free deaths with endemic CollectVio and NatDis
  v_deaths <- dat$Deaths1 + dat$end_colvio + dat$end_natdis + dat$end_othercd + dat$end_diar + dat$end_othercd_prorata
  
  # Calculate fraction of endemic collective violence (Pro-rata squeeze)
  dat$CollectVio <- dat$end_colvio/v_deaths
  
  # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
  dat$NatDis <- dat$end_natdis/v_deaths
  
  if("Diarrhoeal" %in% v_cod){
    # Add endemic othercd crisis to OtherCMPN fraction (Pro-rata squeeze)
    dat$OtherCMPN <- ((dat$OtherCMPN * dat$Deaths1) + dat$end_othercd)/v_deaths
    # Add endemic diar to Diarrhoeal fraction for 5-9, 10-14
    dat$Diarrhoeal <- ((dat$Diarrhoeal * dat$Deaths1) + dat$end_diar)/v_deaths
  }else{
    # otherwise add to OtherCMPN
    dat$OtherCMPN <- ((dat$OtherCMPN * dat$Deaths1) + dat$end_diar + dat$end_othercd)/v_deaths
  }
  
  # Distribute othercd_prorata across othercmpn, LRI, diarrhoeal, TB
  # number of deaths in each cd + proportion of deaths in each other cd * othercd_prorata (number of dths to be added to each) / deaths
  dat[,v_allcd] <- (dat[,v_allcd] * dat$Deaths1 + dat[,v_allcd]/rowSums(dat[,v_allcd]) * dat$end_othercd_prorata) / v_deaths
  
  # After updating modeled fractions (communicable diseases), need to renormalize remaining fractions
  dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis", v_allcd))])] <- 
    dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis", v_allcd))])]/
    rowSums(dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis", v_allcd))])])

  if(UNCERTAINTY){
    
    # Values to be randomly sampled
    v_idSamp <- which(rowSums(dat[, c("CollectVio", "NatDis", v_allcd)]) != 0)
    
    # Sample random values
    if (length(v_idSamp) > 0) {
      
      datAux <- cbind(
        1 - dat$CollectVio[v_idSamp] - dat$NatDis[v_idSamp] - rowSums(dat[v_idSamp, v_allcd]),
        dat$CollectVio[v_idSamp],
        dat$NatDis[v_idSamp],
        dat[v_idSamp, v_allcd, drop = FALSE],
        dat$Deaths1[v_idSamp]
      )
      
      # Randomly sample from crisis-free deaths with probability equivalent to the three CSMFs
      datAux_out <- t(apply(datAux, 1, 
                            function(x){
                              n <- round(x[length(x)]) # total count
                              probs <- x[1:(length(x) - 1)] # probability vector
                              rmultinom(n = 1, size = n, prob = probs)
                            }))
      datAux_out <- as.data.frame(datAux_out)
      names(datAux_out) <- c("resid_prop", "CollectVio", "NatDis", v_allcd)
      
      # Update the Collective Violence and Natural Disasters fractions for the current draw
      dat$CollectVio[v_idSamp] <- datAux_out[, "CollectVio"]/dat$Deaths1[v_idSamp]
      dat$NatDis[v_idSamp] <- datAux_out[, "NatDis"]/dat$Deaths1[v_idSamp]
      # Update all causes in v_allcd
      dat[v_idSamp, v_allcd] <- datAux_out[, v_allcd] / dat$Deaths1[v_idSamp]
      
    }
  }
  
  # Squeeze other causes into remaining fraction
  cod_to_adjust <- v_cod[!v_cod %in% c("CollectVio", "NatDis", v_allcd)]
  # adjustment factor (subtract CollectVio, NatDis, and all v_allcd cols)
  adj_factor <- 1 - dat$CollectVio - dat$NatDis - rowSums(dat[, v_allcd, drop = FALSE])
  dat[, cod_to_adjust] <- dat[, cod_to_adjust] * adj_factor
  
  return(dat)

}
