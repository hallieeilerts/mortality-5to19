fn_pr2 <- function(PA, NPD, MODEL, KEY_CTRYCLASS, ID="pid", PE="any", PLW=1){
  
  #' @title Prediction function
  # 
  #' @description Predictions with an array of coefficients from fn_par()
  #
  #' @param PA object produced with function fn_par()
  #' @param NPD Name of data set with covariates to be used in the prediction
  #' @param MODEL HMM or LMM
  #' @param ID variable in NPD that uniquely identify observations
  #' @param PE Period label ("early","late","any") (for under-5s)
  #' @param PLW distinguishes between preterm and low birth weight as COD (1/0) (for under-5s)
  #' @return Predicted fractions with fixed and fixed+random effects
  
  # Subset prediction data to high or low mortality countries
  v_ctries <- subset(KEY_CTRYCLASS, Group2010 %in% MODEL)$iso3
  NPD <- subset(NPD, iso3 %in% v_ctries)
  
  ## This function makes predictions with fixed effects only and then adds 
  ## ONE random effect term selected at random among some candidates in RT in each MCMC iteration.
  # Prepare prediction data
  VXN <- names(PA$st.data$xmeans)  # vector of numerical covariates
  VXF <- dimnames(PA$BM)[[2]]  # vector of all covariates
  S   <- dim(NPD)[1]  # number of data points to predict
  K   <- length(VXF) # number of covariates including intercept
  H   <- length(VXN) # number of numerical covariates scaled
  C   <- dim(PA$BM)[3] # number of causes of death
  N   <- dim(PA$BM)[1] # number of simulations
  RISO <- dimnames(PA$RM)[[2]] # Countries with random effects in the model
  
  # Prepare raw variables dataset from prediction sample
  DX <- NPD %>%
    dplyr::select(all_of(c(ID, "iso3","year", VXF)))
  # Scale the numerical columns with means and SD from model
  DX[,VXN] <- scale(DX[,VXN], PA$st.data$xmeans, PA$st.data$xsd)
  
  # Point estimates with the MEANS of the coefficients
  POE <- cbind(DX[,1:3], as.matrix(DX[,VXF]) %*% PA$MEB) %>% 
    pivot_longer(cols=colnames(PA$MEB), names_to="cod", values_to="pe.me") %>%
    group_by(pid) %>% 
    mutate(pe.me=exp(pe.me)/sum(exp(pe.me))) %>%
    ungroup()
  # Add Point estimates with the MEDIANS of the coefficients
  POE <- cbind(DX[,1:3], as.matrix(DX[,VXF]) %*% PA$Q2B) %>% 
    pivot_longer(cols=colnames(PA$Q2B), names_to="cod", values_to="pe.q2") %>%
    group_by(pid) %>% 
    mutate(pe.q2=exp(pe.q2)/sum(exp(pe.q2))) %>%
    ungroup() %>%
    left_join(POE)

  # Edit for 5-19: replace random effect for IND in PA$RM with 
  # weighted average of state random effects for India
  if("IND" %in% v_ctries){
    
    # grep names of random effects that begin with I
    v_indst <- row.names(PA$RM[1,,])[grep("^I",row.names(PA$RM[1,,]))]
    # retain those with lower case characters (Indian states)
    v_indst <- v_indst[grepl("[a-z]", v_indst)] 
    
    # select RE for India as a whole and India states
    RM_ind <- PA$RM[, dimnames(PA$RM)[[2]] %in% c(v_indst, "IND"), ]
    
    # Subset India data points in studies
    df_studies_ind <- subset(PA$st.input$studies, reterm %in% c(v_indst, "IND"))[,c("recnr", "reterm")]
    # subset India data points total deaths
    df_totdeaths <- subset(PA$st.input$deaths, recnr %in% df_studies_ind$recnr)[,c("recnr", "totdeaths")]
    df_totdeaths <- df_totdeaths[!duplicated(df_totdeaths),]
    # merge total deaths onto studies
    df_studies_ind <- merge(df_studies_ind, df_totdeaths, by = "recnr")
    # calculate distribution of deaths across national-level studies and states
    df_wt <- df_studies_ind %>%
      group_by(reterm) %>%
      dplyr::summarise(dth = sum(totdeaths)) %>%
      ungroup() %>%
      mutate(total = sum(dth),
             wt = dth/total)
    
    # ensure order of reterms in weights vector matches the order of RM_ind
    # save weights in this order
    v_wt <- df_wt$wt[ match(dimnames(RM_ind)[[2]], df_wt$reterm) ]
    
    # across all slices of RE array (11 for baseline+COD categories), 
    # multiply each state's RE values by its weight
    RM_ind_weighted <- sweep(RM_ind, 2, v_wt, FUN = "*")
    # sum over states (dimension 2)
    RM_ind_sum <- apply(RM_ind_weighted, c(1, 3), sum)
    
    # Replace original IND in random effects array for prediction
    PA$RM[, dimnames(PA$RM)[[2]] %in% "IND", ] <- RM_ind_sum
  }
  
  # Extend Random Effects Matrix to countries in prediction data (RX)
  REX  <- unique(DX$iso3) # countries in prediction data
  # Edit for 5-19y: 
  # RISO contains countries with random effects in model
  # For prediction in 5-19 HMM, we only want to use the country RE for countries with a nationally representative study data point. Otherwise, use an average random effect.
  # further limit RISO 
  if(MODEL == "HMM"){
    v_natrep <- unique(subset(PA$st.input$studies, nationalrep == 1)$iso3)
    RISO <- RISO[RISO %in% v_natrep]
  }
  REXi <- REX[REX %in% RISO] # countries with RE
  REXo <- REX[!(REX %in% RISO)] # countries without RE
  
  # loop through number of mcmc samples
  for (i in 1:N){
    # First calculate random effects of countries in estimation sample
    LOi <- as.data.frame(PA$RM[i, REXi, ])
    LOi <- cbind(iso3 = rownames(LOi), LOi)
    row.names(LOi) <- NULL
    # add randomly drawn random effects for countries not in estimation sample
    LOi <- LOi %>%
      bind_rows(cbind(data.frame(iso3=REXo), mvrnorm(length(REXo), rep(0,C), cov(PA$RM[i,,])))) %>% 
      pivot_longer(cols=dimnames(PA$RM)[[3]], names_to="cod", values_to="ref")
    # Calculate fixed effects and add the random effects:
    LOi <- cbind(DX[,1:3], as.matrix(DX[,VXF]) %*% PA$BM[i,,]) %>% 
      pivot_longer(cols=dimnames(PA$BM)[[3]], names_to="cod", values_to="fef") %>%
      left_join(LOi) %>% mutate(sample=i)
    if(i==1) LO <- LOi else LO <- bind_rows(LO, LOi) 
  }
  LO <- mutate(LO, fref = fef + ref) %>%
    group_by(pid, sample) %>% 
    mutate(pf=exp(fef)/sum(exp(fef)), 
           pr=exp(fref)/sum(exp(fref))) %>%
    ungroup()
  
  return(list(Point_estimates=POE, 
              Predictions=LO[,c("pid","iso3","year","cod", "sample","pf","pr")], 
              Prediction.Data=NPD, 
              st.input=PA$st.input))
}


