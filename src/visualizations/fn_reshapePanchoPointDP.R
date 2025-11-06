fn_reshapePanchoPointDP <- function(DAT, KEY_CTRYCLASS, CODALL, AGESEXSUFFIX){
  
  #' @title Reshape Pancho's national point estimate results from data portal format
  # 
  #' @description 
  #
  #' @param DAT 
  #' @return Results formatted to match my modeled/squeezed/results formatted output

  # # testing
  # DAT <- point_PanchoResults
  # KEY_CTRYCLASS <- key_ctryclass
  # CODALL <- codAll
  
  dat <- DAT
  key <- KEY_CTRYCLASS[,c("iso3", "Group2010")]
  
  # keep only fractions
  dat <- subset(dat, Indicator == "Fraction")
  
  names(dat)[which(names(dat) == "REF_AREA")] <- "ISO3"
  names(dat)[which(names(dat) == "TIME_PERIOD")] <- "Year"
  names(dat)[which(names(dat) == "SEX")] <- "Sex"
  
  # Merge on country key
  names(key) <- c("ISO3", "Model")
  dat <- merge(dat, key, by = "ISO3")
  
  # rename CODs and reshape wide
  dat <- dat %>%
    mutate(Cause.of.death = case_when(
      #Cause.of.death == "Cardiovascular" ~ "Cardiovascular",
      Cause.of.death == "Collective violence" ~ "CollectVio",
      Cause.of.death == "Congenital anomalies" ~ "Congenital",
      Cause.of.death == "Diarrhea" ~ "Diarrhoeal",
      Cause.of.death == "Digestive system" ~ "Digestive",
      #Cause.of.death == "Drowning" ~ "Drowning",
      Cause.of.death == "HIV/AIDS" ~ "HIV",
      Cause.of.death == "Interpersonal violence" ~ "InterpVio",
      Cause.of.death == "Lower respiratory infections" ~ "LRI",
      Cause.of.death == "Maternal causes" ~ "Maternal",
      Cause.of.death == "Natural disasters" ~ "NatDis",
      Cause.of.death == "Neoplasms/cancer" ~ "Neoplasms",
      Cause.of.death == "Other communicable diseases" ~ "OtherCMPN",
      Cause.of.death == "Other injuries" ~ "OtherInj",
      Cause.of.death == "Other NCDs" ~ "OtherNCD",
      Cause.of.death == "Road traffic injuries" ~ "RTI",
      Cause.of.death == "Self-harm" ~ "SelfHarm",
      Cause.of.death == "Tuberculosis" ~ "TB",
      TRUE ~ Cause.of.death),
    OBS_VALUE = OBS_VALUE/100,
    AgeLow = case_when(
      Age.group == "5 to 9 years" ~ 5,
      Age.group == "10 to 14 years" ~ 10,
      Age.group == "15 to 19 years" ~ 15,
      TRUE ~ NA),
    AgeUp = case_when(
      Age.group == "5 to 9 years" ~ 9,
      Age.group == "10 to 14 years" ~ 14,
      Age.group == "15 to 19 years" ~ 19,
      TRUE ~ NA)
    ) %>%
    pivot_wider(
      id_cols = c("ISO3", "Year", "Sex", "AgeLow", "AgeUp", "Model"),
      names_from = "Cause.of.death",
      values_from = "OBS_VALUE"
    )
  
  # order COD
  v_cod <- CODALL[CODALL %in% names(dat)]
  dat <- dat %>%
    select(ISO3, Year, Sex, AgeLow, AgeUp, Model, all_of(v_cod))

  # select sex if sex-specific
  if(AGESEXSUFFIX == "15to19yF"){
    dat <- dat %>% filter(Sex == "Female")
  }
  if(AGESEXSUFFIX == "15to19yM"){
    dat <- dat %>% filter(Sex == "Male")
  }
  
  
  return(dat)
  
}

