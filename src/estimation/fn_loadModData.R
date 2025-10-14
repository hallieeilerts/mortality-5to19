fn_loadModData <- function(AGESEXSUFFIX, MODEL){
  
  #' @title Load model data
  # 
  #' @description Loads studies and deaths for LMM or HMM
  #
  #' @param AGESEXSUFFIX ageSexSuffix as set in prepare-session
  #' @param MODEL "HMM" or "LMM"
  #' @return List with data frames for studies and deaths
  
  # Deaths
  dat_filename <- list.files("./data/model-objects/")
  dat_filename <- dat_filename[grepl("deaths", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(MODEL, dat_filename)] 
  dat_filename <- dat_filename[grepl(AGESEXSUFFIX, dat_filename)] 
  load(paste0("./data/model-objects/", dat_filename, sep = ""))

  # Studies
  dat_filename <- list.files("./data/model-objects/")
  dat_filename <- dat_filename[grepl("studies", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(MODEL, dat_filename)] 
  dat_filename <- dat_filename[grepl(AGESEXSUFFIX, dat_filename)] 
  load(paste0("./data/model-objects/",dat_filename, sep = ""))
  # In LMM, studies has upper case ISO3
  studies <- studies %>%
    rename(iso3 = any_of("ISO3"))
  
  # Combine
  l_dat <- list(deaths, studies)
  names(l_dat) <- c("deaths", "studies")
  
  return(l_dat)

}
