fn_capMalFrac <- function(CSMF, DAT_MALARIA_05to19, FRAC_MALARIA_01to04){
  
  #' @title Cap malaria fractions
  # 
  #' @description Caps malaria fractions at country-year level for 1-4y.
  #
  #' @param CSMF Data frame with predicted CSMFs.
  #' @param DAT_MALARIA_05to19 Data frame with malaria case counts for 5-19y
  #' @param FRAC_MALARIA_01to04 Data frame with final estimated CSMF for malaria for 1-4y ("csmf_malaria_01to04")
  #' @return Data frame where predicted CSMFs have been adjusted for capped malaria fractions.
  
  # Identify COD
  v_cod <- names(CSMF)
  v_cod <- v_cod[!(v_cod %in% idVars)]
  
  ### Merge on malaria cases
  
  dat <- merge(CSMF, DAT_MALARIA_05to19, by = c("iso3", "year"), all.x = T)
  
  # Identify countries with 0 malaria
  idMal <- which(dat$cases_malaria_05to19 == 0 | is.na(dat$cases_malaria_05to19))
  
  # Force malaria to be 0
  if (length(idMal) > 0) {
    dat[idMal, paste(v_cod)] <- dat[idMal, paste(v_cod)]/rowSums(dat[idMal, paste(v_cod[v_cod != "Malaria"])])
    dat[idMal, "Malaria"] <- 0
  }
  
  # Remove unnecessary columns
  v_remove <- names(DAT_MALARIA_05to19)[!(names(DAT_MALARIA_05to19) %in% idVars)]
  dat <- dat[, !(names(dat) %in% v_remove)]
  
  ### Merge on malaria 1-59 month CSMF
  
  dat <- merge(dat, FRAC_MALARIA_01to04, by = c("iso3", "year"), all.x = T)
  
  # Assign 0 to NA (if any)
  dat$csmf_malaria_01to04[which(is.na(dat$csmf_malaria_01to04))] <- 0
  
  # Identify cases to update malaria
  idMal <- which(dat$Malaria > dat$csmf_malaria_01to04)
  
  # Cap malaria
  if (length(idMal) > 0) {
    dat$Malaria[idMal] <- dat$csmf_malaria_01to04[idMal]
    idCod <- v_cod[v_cod != "Malaria"]
    dat[idMal, paste(idCod)] <- dat[idMal, paste(idCod)] / rowSums(dat[idMal, paste(paste(idCod))])
    dat[idMal, paste(idCod)] <- dat[idMal, paste(idCod)] * (1 - dat$Malaria[idMal])
    rm(idCod)
  }
  
  # Remove unnecessary data
  v_remove <- names(FRAC_MALARIA_01to04)[!(names(FRAC_MALARIA_01to04) %in% idVars)]
  dat <- dat[, !(names(dat) %in% v_remove)]
  
  # Tidy up
  dat <- dat[order(dat$iso3, dat$year),]
  
  return(dat)
  
}
