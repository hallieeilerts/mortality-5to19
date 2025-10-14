fn_setRefCat <- function(AGESEXSUFFIX, CTRYGRP){
  
  if(CTRYGRP == "HMM"){
    if (AGESEXSUFFIX == "05to09y") {
      refCat <- 'Diarrhoeal' 
      
    }
    
    if (AGESEXSUFFIX == "10to14y") {
      refCat <- 'LRI' 
      
    }
    
    if (AGESEXSUFFIX == "15to19yF") {
      refCat <- 'SelfHarm' 
    }  
    
    if (AGESEXSUFFIX == "15to19yM") {
      refCat <- 'RTI' 
      
    }  
  }
  
  
  
  if(CTRYGRP == "LMM"){
    if (AGESEXSUFFIX == "05to09y") {
      refCat <- "Neoplasms"
      
    }
    
    if (AGESEXSUFFIX == "10to14y") {
      refCat <- "Neoplasms"
      
    }
    
    if (AGESEXSUFFIX == "15to19yF") {
      refCat <- "RTI"
    }  
    
    if (AGESEXSUFFIX == "15to19yM") {
      refCat <- "RTI"
      
    }  
  }
  
  return(refCat)
}


