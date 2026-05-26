fn_combineUIpoint <- function(UI, CSMFPOINT, CODALL, REGIONAL = FALSE){
  
  #' @title Combine raw data frames for CSMF point estimates, uncertainty lower and upper bounds (drop median).
  # 
  #' @description Transforms CSMF point estimates into rates and deaths as well, combines with uncertainty intervals for CSMFs, rates, and deaths, orders rows and columns.
  #
  #' @param CSMFPOINT Data frame with CSMFs that have been processed in squeezing pipeline (contains all countries, even those not subject to squeezing).
  #' @param UI     Data frame with lower and upper quantiles for each COD for deaths, fractions, rates.
  #' @param KEY_CODLIST Data frame with age-specific CODs
  #' @return Data frame with point estimates, lower, and upper bounds for CSMFs, deaths, and rates.
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- subset(KEY_CODLIST, ModeledOrReported == "Reported")$COD

  
  # ID variables
  if(!REGIONAL){
    idVars <- c("iso3", "year")
  }else{
    idVars <- c("Region", "year")
  }
  
  # Only keep the following columns
  csmfPoint <- CSMFPOINT[,c(idVars, "Deaths2", "Rate2", v_cod)]
  
  df_frac  <- csmfPoint
  df_rates <- csmfPoint
  df_deaths <- csmfPoint
  df_rates[,v_cod]  <- csmfPoint[,v_cod] * csmfPoint[,"Rate2"]
  df_deaths[,v_cod] <- csmfPoint[,v_cod] * csmfPoint[,"Deaths2"]
  
  # Add columns identifying point estimates
  df_frac$Variable   <- "Fraction"
  df_rates$Variable  <- "Rate"
  df_deaths$Variable <- "Deaths"
  df_frac$Quantile   <- "Point"
  df_rates$Quantile  <- "Point"
  df_deaths$Quantile <- "Point"
  
  # Keep same columns in UI
  v_col <- names(df_frac)[names(df_frac) %in% names(UI)]
  ui <- UI[,paste(v_col)]
  # Only keep upper and lower bounds (drop median)
  ui <- subset(ui, Quantile %in% c("Lower", "Upper"))
  
  # Combine and tidy
  df_res <- rbind(df_frac, df_rates, df_deaths, ui)
  df_res <- df_res[, c(idVars, "Deaths2", "Rate2", "Variable", "Quantile", v_cod)]
  if(!REGIONAL){
    df_res <- df_res[order(df_res$iso3, df_res$year, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$year, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL
  
  return(df_res)
  
}
