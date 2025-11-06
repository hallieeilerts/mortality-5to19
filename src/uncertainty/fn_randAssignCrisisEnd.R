
fn_randAssignCrisisEnd <- function(CSMFDRAW, KEY_CODLIST){
  
  #' @title Randomly assign endemic crisis deaths for current draw
  # 
  #' @description If there are any endemic crisis deaths, sample from multinomial distribution.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled endemic crisis deaths.
  
  # # testing
  # CSMFDRAW <- csmfDraws_singlecauseADD[[1]]
  # KEY_CODLIST <- key_codlist
  
  dat <- CSMFDRAW
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD
  
  # Other communicable causes in this age group
  v_allcd <- c("OtherCMPN", "LRI", "Diarrhoeal", "TB")
  v_allcd <- v_allcd[v_allcd %in% v_cod]
  
  # Add crisis-free deaths with endemic CollectVio and NatDis
  #v_deaths <- dat$Deaths1 + dat$CollectVio + dat$NatDis
  v_deaths <- dat$Deaths1 + dat$end_colvio + dat$end_natdis + dat$end_othercd + dat$end_diar + dat$end_othercd_prorata
  
  # Calculate fraction of endemic collective violence (Pro-rata squeeze)
  #dat$CollectVio <- dat$CollectVio/v_deaths
  dat$CollectVio <- dat$end_colvio/v_deaths
  
  # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
  #dat$NatDis <- dat$NatDis/v_deaths
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
  
  # Distribute othercd_prorata across communicable causes
  # OtherCMPN, LRI, Diarrhoeal are already fractions
  # TB will need to be temporarily converted to one.
  
  # number of deaths in each cd + proportion of deaths in each other cd * othercd_prorata (number of dths to be added to each) / deaths
  dat[,v_allcd] <- (dat[,v_allcd] * dat$Deaths1 + dat[,v_allcd]/rowSums(dat[,v_allcd]) * dat$end_othercd_prorata) / v_deaths
  
  # After updating modeled fractions (communicable diseases), need to renormalize remaining fractions
  dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis", v_allcd))])] <- 
    dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis", v_allcd))])]/
    rowSums(dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis", v_allcd))])])
  
  # Values to be squeezed
  #v_idSqz <- which(dat$CollectVio != 0 | dat$NatDis != 0)
  v_idSqz <- which(rowSums(dat[, c("CollectVio", "NatDis", v_allcd)]) != 0)
  
  # Sample random values
  if (length(v_idSqz) > 0) {
    # Create data.frame with csmfs for inverse of endemic crisis, CollectVio, NatDis, and crisis-free deaths
    # datAux <- cbind(1 - dat$CollectVio[v_idSqz] - dat$NatDis[v_idSqz],
    #                 dat$CollectVio[v_idSqz], 
    #                 dat$NatDis[v_idSqz], 
    #                 dat$Deaths1[v_idSqz])
    datAux <- cbind(
      1 - dat$CollectVio[v_idSqz] - dat$NatDis[v_idSqz] - rowSums(dat[v_idSqz, v_allcd]),
      dat$CollectVio[v_idSqz],
      dat$NatDis[v_idSqz],
      dat[v_idSqz, v_allcd, drop = FALSE],
      dat$Deaths1[v_idSqz]
    )
    
    # Randomly sample from crisis-free deaths with probability equivalent to the three CSMFs
    datAux <- t(apply(datAux, 1,
                      function(x) {
                        rmultinom(n = 1, size = round(x[ncol(x)]), prob = x[1:(ncol(x)-1)])
                      }))
    # Update the Collective Violence and Natural Disasters fractions for the current draw
    dat$CollectVio[v_idSqz] <- datAux[, 2]/dat$Deaths1[v_idSqz]
    dat$NatDis[v_idSqz] <- datAux[, 3]/dat$Deaths1[v_idSqz]
    # Update all causes in v_allcd
    dat[v_idSqz, v_allcd] <- datAux[, v_allcd] / dat$Deaths1[v_idSqz]
  }
  
  return(dat)
}


#' fn_randAssignCrisisEnd <- function(CSMFDRAW){
#'   
#'   #' @title Randomly assign endemic crisis deaths for current draw
#'   # 
#'   #' @description If there are any endemic crisis deaths, sample from multinomial distribution.
#'   #' 
#'   #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
#'   #' @return Data frame with randomly sampled endemic crisis deaths.
#'   
#'   dat <- CSMFDRAW
#'   
#'   # Add crisis-free deaths with endemic CollectVio and NatDis
#'   v_deaths <- dat$Deaths1 + dat$CollectVio + dat$NatDis
#'   
#'   # Calculate fraction of endemic collective violence (Pro-rata squeeze)
#'   dat$CollectVio <- dat$CollectVio/v_deaths
#'   
#'   # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
#'   dat$NatDis <- dat$NatDis/v_deaths
#'   
#'   # Values to be squeezed
#'   v_idSqz <- which(dat$CollectVio != 0 | dat$NatDis != 0)
#'   
#'   # Sample random values
#'   if (length(v_idSqz) > 0) {
#'     # Create data.frame with csmfs for inverse of endemic crisis, CollectVio, NatDis, and crisis-free deaths
#'     datAux <- cbind(1 - dat$CollectVio[v_idSqz] - dat$NatDis[v_idSqz],
#'                     dat$CollectVio[v_idSqz], dat$NatDis[v_idSqz], 
#'                     dat$Deaths1[v_idSqz])
#'     # Randomly sample from crisis-free deaths with probability equivalent to the three CSMFs
#'     datAux <- t(apply(datAux, 1,
#'                       function(x) {
#'                         rmultinom(n = 1, size = round(x[4]), prob = x[1:3])
#'                       }))
#'     # Update the Collective Violence and Natural Disasters fractions for the current draw
#'     dat$CollectVio[v_idSqz] <- datAux[, 2]/dat$Deaths1[v_idSqz]
#'     dat$NatDis[v_idSqz] <- datAux[, 3]/dat$Deaths1[v_idSqz]
#'   }
#'   
#'   return(dat)
#' }
#' 
