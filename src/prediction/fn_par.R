##################################################Z
####
####   Gather model data from stanfit object, for prediction
####
fn_par <- function(MO, NP=500){
  ## MO    Stan object with posterior MCMC coefficient distribution
  ## NP    Number of coefficient sets from which to estimate credible intervals
  # Recover simulation parameters from Stan output
  
  # testing
  #MO <- mod_fit_HMM
  #NP <- 500
  
  SA <- rstan::extract(MO$st.output) 
  
  # selected NP iterations at random
  SS <- sample(dim(SA$B)[1], NP) 
  
  # Means of beta-parameters (add column for reference COD)
  MEB <- cbind(rep(0, dim(SA$B)[2]), apply(SA$B,c(2,3),mean))
  dimnames(MEB) <- list(dimnames(MO$st.data$Xmat)[[2]],
                        dimnames(MO$st.data$Missreport)[[2]])
  # Medians of beta-parameters (add column for reference COD)
  Q2B <- cbind(rep(0, dim(SA$B)[2]), apply(SA$B,c(2,3),median))
  dimnames(Q2B) <- list(dimnames(MO$st.data$Xmat)[[2]],
                        dimnames(MO$st.data$Missreport)[[2]])
  # prepare array of fixed effects
  BM <- SA$B[SS,,]
  # add 0 coefficients for reference cause of death
  BM <- abind(array(0, dim=dim(BM)[1:2]), BM)
  # Add names to matrix
  dimnames(BM) <- list(c(1:NP),
                       dimnames(MO$st.data$Xmat)[[2]],
                       dimnames(MO$st.data$Missreport)[[2]])
  # prepare array of random effects
  RM <- SA$re[SS,,]
  # Add 0 random effect for reference cause of death
  RM <- abind(array(0, dim=dim(RM)[1:2]), RM)
  # Add names to matrix
  dimnames(RM) <- list(c(1:NP),
                       MO$st.data$Rnames,
                       dimnames(MO$st.data$Missreport)[[2]])
  # prepare array of RESD
  SM <- SA$sd_re[SS,]
  # Add 0 random effect SD for reference cause of death
  SM <- cbind(rep(0, NP), SM)
  # Add names to matrix
  dimnames(SM) <- list(c(1:NP),
                       dimnames(MO$st.data$Missreport)[[2]])
  
  return(list(BM=BM, RM=RM, SM=SM, MEB=MEB, Q2B=Q2B, st.input=MO$st.input, st.data=MO$st.data))
}





# Previous version of function
# Deprecated as of September 2, 2025

# ##################################################Z
# ####
# ####   Gather model data from stanfit object, for prediction
# ####
# fn_par <- function(MO, NP=500){
#   ## MO    Stan object with posterior MCMC coefficient distribution
#   ## NP    Number of coefficient sets from which to estimate credible intervals
#   # Recover simulation parameters from Stan output
# 
#   # testing
#   #MO <- mod_fit_HMM
#   #NP <- 500
# 
#   SA <- rstan::extract(MO$st.output)
# 
#   # selected NP iterations at random
#   SS <- sample(dim(SA$B)[1], NP)
#   # prepare array of fixed effects
#   BM <- SA$B[SS,,]
#   # add 0 coefficients for reference cause of death
#   BM <- abind(array(0, dim=dim(BM)[1:2]), BM)
#   # Add names to matrix
#   dimnames(BM) <- list(c(1:NP),
#                        dimnames(MO$st.data$Xmat)[[2]],
#                        dimnames(MO$st.data$Missreport)[[2]])
#   # prepare array of random effects
#   RM <- SA$re[SS,,]
#   # Add 0 random effect for reference cause of death
#   RM <- abind(array(0, dim=dim(RM)[1:2]), RM)
#   # Add names to matrix
#   dimnames(RM) <- list(c(1:NP),
#                        MO$st.data$Rnames,
#                        dimnames(MO$st.data$Missreport)[[2]])
#   # prepare array of RESD
#   SM <- SA$sd_re[SS,]
#   # Add 0 random effect SD for reference cause of death
#   SM <- cbind(rep(0, NP), SM)
#   # Add names to matrix
#   dimnames(SM) <- list(c(1:NP),
#                        dimnames(MO$st.data$Missreport)[[2]])
# 
#   return(list(BM=BM, RM=RM, SM=SM, st.input=MO$st.input, st.data=MO$st.data))
# }
