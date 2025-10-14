fn_createModInput <- function(AGESEXSUFFIX, MODEL, MOD_DAT, DAT_COVAR, DAT_COD, DAT_HP){
  
  #' @title Call function for fn_e1() - model estimation
  # 
  #' @description Runs fn_e1 and saves fit
  #
  #' @param MOD_DAT List with studies and deaths
  #' @param AGESEXSUFFIX ageSexSuffix
  #' @param MODEL HMM or LMM
  #' @param DAT_COVAR key with age/model-specific CODs
  #' @param DAT_COD key with age/model-specific covariates
  #' @param DAT_HP key with age/model-specific hyperparameters
  #' @return List with data for fitting model
  
  # # testing
  # AGESEXSUFFIX <- ageSexSuffix
  # MODEL <- "LMM"
  # MOD_DAT <-  mod_dat_LMM
  # DAT_COVAR <- dat_covar
  # DAT_COD <- dat_cod
  # DAT_HP <- dat_hp

  # Model data
  studies <- MOD_DAT$studies
  deaths <- MOD_DAT$deaths
  
  # Prepare deaths
  # Add row number
  deaths <- deaths %>%
    mutate(row = 1:n())

  # Prepare studies
  # Add index for first and last row to studies
  # Create reterm from iso3 and india_state23
  # Create reid, intercept
  studies <- studies %>%
    arrange(recnr) %>%
    left_join(deaths %>% group_by(recnr) %>% summarise(first=min(row), last=max(row)) %>% ungroup(), by = "recnr") %>%
    mutate(reterm = ifelse(iso3 == "IND", india_state23, iso3),
           reterm = factor(reterm, levels = sort(unique(reterm))),
           reid = as.numeric(factor(reterm, levels = sort(unique(reterm)))),
           intercept = 1)
  
  # COD vector
  v_cod_modeled <- subset(DAT_COD, ModeledOrReported == MODEL & AgeSexSuffix == AGESEXSUFFIX)$COD
  refCat <- fn_setRefCat(AGESEXSUFFIX, MODEL)
  vdt <- as.factor(c(refCat, paste(v_cod_modeled[v_cod_modeled != refCat])))
  
  # Covariate vectors
  vxc <- NULL
  vxf <- subset(DAT_COVAR, Model == MODEL & AgeSexSuffix == AGESEXSUFFIX)$Covariate
  vxn <- vxf
  
  # Keep only required columns
  if(MODEL == "HMM"){
    studies <- studies %>%
      dplyr::select(all_of(c("recnr", "strata_id", "iso3", "reterm", "nationalrep", "first", "last", "intercept", vxc, vxn)))
    # Number of non covariate/intercept columns
    vextra <- 7
  }
  if(MODEL == "LMM"){
    studies <- studies %>%
      dplyr::select(all_of(c("recnr", "iso3", "reterm", "first", "last", "intercept", vxc, vxn))) ### NOTE: IF NECESSARY, MAKE IT RETERM INSTEAD OF REID
    # Number of non covariate/intercept columns
    vextra <- 5
  }
  
  # Dropping columns from deaths to only keep binary reclassification matrix
  deaths_reclassM <- deaths %>%
    dplyr::select(all_of(c(as.character(vdt))))
  
  # Hyperparameters
  resd <- subset(DAT_HP, Model == MODEL & AgeSexSuffix == AGESEXSUFFIX)$sdre
  lam <- subset(DAT_HP, Model == MODEL & AgeSexSuffix == AGESEXSUFFIX)$lamb
  
  # Create st.input object for stan (check the hyperparameters and other options..!!)
  st_input <- list(
    model = stan_model(file='./src/estimation2025/cacode_DP.stan', auto_write = TRUE),
    studies = studies,
    deaths = deaths,
    lambda = lam,   # lambda for lasso
    rsdlim = resd,  # define max SD of RE 
    sdbeta = 0.5,   # SD of parameters not in Lasso
    vdt = vdt,
    refcat = refCat,
    vxn = vxn,
    vxc = vxc,
    nsv = 1, # number of parameters outside Lasso (just intercept)
    param = c('B', 're', 'sd_re'),
    nchai = 4,
    niter = 4000,
    nwarm = 2000,
    cores = 4,
    patho = here("./gen/estimation2025/output/"), # path to store model output
    name = paste0("Stan_", MODEL, "_", AGESEXSUFFIX, "_", resd, "_", lam), # name of the model
    summary = 0  # put 0 if you want to save all the posterior samples of the parameters to estimate, or 1 if you only want to save the summaries of the posterior distributions  
  )
  
  # Data list for Stan
  st_data <- list(
    # Studies
    nStudy = nrow(studies),
    K = ncol(studies) - vextra, # minus all non covariate/intercept columns
    # matrix of covariates
    Xmat = cbind(as.matrix(studies[,c("intercept", vxc)]), scale(as.matrix(studies[, vxn]))),
    nNoshrink = st_input$nsv,
    first = studies$first,
    last = studies$last,
    # random effects
    nre = length(unique(studies$reterm)),
    reid = as.numeric(studies$reterm),
    Rnames = levels(studies$reterm), #names of Reffects
    # Deaths
    nCause = ncol(deaths_reclassM), 
    nRows = nrow(deaths_reclassM),
    Missreport = as.matrix(deaths_reclassM),
    nDeaths = deaths$n,
    # Parameters
    sd_betareg_noshrink =  st_input$sdbeta, # SD of parameters not in Lasso
    rsdlim = st_input$rsdlim,               # define max SD of RE 
    lambda = st_input$lambda,               # lambda for lasso
    # Means and SD standardised covariates
    xmeans = apply(studies[,vxn], 2, mean, na.rm=T),
    xsd = apply(studies[,vxn], 2, sd, na.rm=T)
  )
  # Hallie edit: add intercept column name to xmat. need to do because 5-19 has no vxc covariates and "intercept" column name is subsequently empty.
  colnames(st_data$Xmat) <- c("intercept", vxc, vxn)
  
  # Create model input
  mod_input <- list()
  mod_input[["ageSexSuffix"]] <- AGESEXSUFFIX
  mod_input[["model"]] <- MODEL
  #mod_input[["deaths"]] <- deaths
  #mod_input[["studies"]] <- studies
  #mod_input[["vxc"]] <- vxc
  #mod_input[["vxf"]] <- vxf
  #mod_input[["vxn"]] <- vxn
  #mod_input[["vdt"]] <- vdt
  #mod_input[["refCat"]] <- refCat
  #mod_input[["lam"]] <- lam
  #mod_input[["resd"]] <- resd
  mod_input[["st_input"]] <- st_input
  mod_input[["st_data"]] <- st_data

  return(mod_input)
  
}
