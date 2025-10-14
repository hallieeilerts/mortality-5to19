################################################ #
####
####   Predictions with an array of coefficients from f.par()
####   
fn_pr2 <- function(PA, NPD, MODEL, KEY_CTRYCLASS, ID="pid", PE="any", PLW=1){
  
  ## PA   object produced with function f.par()
  ## NPD  Name of data set with covariates to be used in the prediction
  ## MODEL HMM or LMM
  ## ID   variable in NPD that uniquely identify observations
  ## PE   Period label ("early","late","any")
  ## PLW  distinguishes between preterm and low birth weight as COD (1/0)
  
  # # testing
  # PA <- mod_mat_HMM
  # NPD <- dat_pred
  # MODEL <- "HMM"
  # KEY_CTRYCLASS <- key_ctryclass
  # ID <- "pid"
  # PE <- "any"
  # PLW <- 1
  
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
  DX <- mutate(NPD, 
               #per.early=ifelse(PE=="early",1,0),
               #per.late=ifelse(PE=="late",1,0),
               #premvslbw=PLW
  ) %>%
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
    LOi <- rownames_to_column(as.data.frame(PA$RM[i,REXi,]), var="iso3") %>%
      # add randomly drawn random effects for countries not in estimation sample
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
    mutate(pf=exp(fef)/sum(exp(fef)), pr=exp(fref)/sum(exp(fref))) %>%
    ungroup()
  
  return(list(Point_estimates=POE, 
              Predictions=LO[,c("pid","iso3","year","cod", "sample","pf","pr")], 
              Prediction.Data=NPD, 
              st.input=PA$st.input))
}



# Previous version of function
# Deprecated as of September 2, 2025

# ################################################ #
# ####
# ####   Predictions with an array of coefficients from f.par()
# ####   
# fn_pr2 <- function(PA, NPD, MODEL, KEY_CTRYCLASS, ID="pid", PE="any", PLW=1){
#   
#   ## PA   object produced with function f.par()
#   ## NPD  Name of data set with covariates to be used in the prediction
#   ## MODEL HMM or LMM
#   ## ID   variable in NPD that uniquely identify observations
#   ## PE   Period label ("early","late","any")
#   ## PLW  distinguishes between preterm and low birth weight as COD (1/0)
#   
#   # # testing
#   # PA <- mod_mat_HMM
#   # NPD <- dat_pred
#   # MODEL <- "HMM"
#   # KEY_CTRYCLASS <- key_ctryclass
#   # ID <- "pid"
#   # PE <- "any"
#   # PLW <- 1
# 
#   # Subset prediction data to high or low mortality countries
#   v_ctries <- subset(KEY_CTRYCLASS, Group2010 %in% MODEL)$iso3
#   NPD <- subset(NPD, iso3 %in% v_ctries)
#   
#   ## This function makes predictions with fixed effects only and then adds 
#   ## ONE random effect term selected at random among some candidates in RT in each MCMC iteration.
#   # Prepare prediction data
#   VXN <- names(PA$st.data$xmeans)  # vector of numerical covariates
#   VXF <- dimnames(PA$BM)[[2]]  # vector of all covariates
#   S   <- dim(NPD)[1]  # number of data points to predict
#   K   <- length(VXF) # number of covariates including intercept
#   H   <- length(VXN) # number of numerical covariates scaled
#   C   <- dim(PA$BM)[3] # number of causes of death
#   N   <- dim(PA$BM)[1] # number of simulations
#   RISO <- dimnames(PA$RM)[[2]] # Countries with random effects in the model
#   
#   # Prepare raw variables dataset from prediction sample
#   DX <- mutate(NPD, 
#                #per.early=ifelse(PE=="early",1,0),
#                #per.late=ifelse(PE=="late",1,0),
#                #premvslbw=PLW
#                ) %>%
#     dplyr::select(all_of(c(ID, "iso3","year", VXF)))
#   # Scale the numerical columns with means and SD from model
#   DX[,VXN] <- scale(DX[,VXN], PA$st.data$xmeans, PA$st.data$xsd)
#   
#   # Edit for 5-19: Average random effects for India
#   if("IND" %in% v_ctries){
# 
#     v_indst <- row.names(PA$RM[1,,])[grep("^I",row.names(PA$RM[1,,]))]
#     v_indst <- v_indst[grepl("[a-z]", v_indst)] # retain those with lower case characters (Indian states)
# 
#     # RE for India states
#     RM_ind <- PA$RM[, dimnames(PA$RM)[[2]] %in% c(v_indst, "IND"), ]
# 
#     # Subset India data points in studies
#     df_studies_ind <- subset(PA$st.input$studies, reterm %in% c(v_indst, "IND"))[,c("recnr", "reterm")]
#     # Subset their total deaths
#     df_totdeaths <- subset(PA$st.input$deaths, recnr %in% df_studies_ind$recnr)[,c("recnr", "totdeaths")]
#     df_totdeaths <- df_totdeaths[!duplicated(df_totdeaths),]
#     # Merge total deaths onto studies
#     df_studies_ind <- merge(df_studies_ind, df_totdeaths, by = "recnr")
#     # Calculate distribution of deaths across national-level studies and states
#     df_wt <- df_studies_ind %>%
#       group_by(reterm) %>%
#       summarise(dth = sum(totdeaths)) %>%
#       ungroup() %>%
#       mutate(total = sum(dth),
#              wt = dth/total)
# 
#     # Ensure weights vector matches the state dimension of RM_ind
#     v_wt <- df_wt$wt[ match(dimnames(RM_ind)[[2]], df_wt$reterm) ]
# 
#     # Multiply each state's values by its weight
#     RM_ind_weighted <- sweep(RM_ind, 2, v_wt, FUN = "*")
#     # Sum over states (dimension 2)
#     RM_ind_sum <- apply(RM_ind_weighted, c(1, 3), sum)
# 
#     # Replace original IND in random effects array for prediction
#     PA$RM[, dimnames(PA$RM)[[2]] %in% "IND", ] <- RM_ind_sum
#   }
# 
#   # Edit for 5-19y: replace RE for countries that are in the RE matrix but not nationally representative
#   # replace with random sample
#   if(MODEL == "HMM"){
#     
#     v_natrep <- unique(subset(PA$st.input$studies, nationalrep == 1)$iso3)
#     RE_replace <- dimnames(PA$RM)[[2]][!(dimnames(PA$RM)[[2]] %in% v_natrep)]
#     if (length(RE_replace) > 0) {
#       RX_replace <- array(rnorm(length(rep(c(PA$SM), length(RE_replace))), 0, 
#                                 rep(c(PA$SM), length(RE_replace))), 
#                           dim = c(dim(PA$SM), length(RE_replace)),
#                           dimnames = list(dimnames(PA$SM)[[1]], dimnames(PA$SM)[[2]], RE_replace))
#       RX_replace <- aperm(RX_replace, c(1, 3, 2))
#       PA$RM[, RE_replace, ] <- RX_replace
#     }
#   }
#   
#   # Extend Random Effects Matrix to countries in prediction data (RX)
#   REX <- unique(DX$iso3)
#   # Identify countries in prediction data but not the estimated RE matrix
#   REX <- REX[!(REX %in% dimnames(PA$RM)[[2]])]
#   # Draw randomly simulated RE values for new countries, using the same SDs estimated from the model.
#   RX <- array(rnorm(length(rep(c(PA$SM),length(REX))), 0, rep(c(PA$SM),length(REX))), 
#               dim=c(dim(PA$SM), length(REX)),
#               dimnames=list(dimnames(PA$SM)[[1]], dimnames(PA$SM)[[2]], REX))
#   RX <- aperm(RX, c(1,3,2))
#   # array combining random effect for in-sample and out-of-sample RE
#   RT <- abind(PA$RM, RX, along = 2)
#   
#   # # Prepare matrix to store predictions
#   LF <- array(NA, dim = c(N,S,C))  # matrix of fixed Predictions
#   dimnames(LF) <- list(dimnames(PA$BM)[[1]], NPD[[ID]], dimnames(PA$BM)[[3]])
#   LR <- LF  # matrix of random Predictions
#   for (i in 1:N){
#     LF[i,,] <- as.matrix(DX[,VXF]) %*% PA$BM[i,,] # logodds with fixed effects
#     LR[i,,] <-  LF[i,,] + RT[i,match(NPD[["iso3"]], dimnames(RT)[[2]]),]
#   }  
#   PF <- aperm(apply(LF, c(1,2), function(x) exp(x)/sum(exp(x))), perm=c(2,3,1))
#   PR <- aperm(apply(LR, c(1,2), function(x) exp(x)/sum(exp(x))), perm=c(2,3,1))
#   # return:
#   return(list(PF=PF, PR=PR, Prediction.Data=NPD, st.input=PA$st.input))
# }
