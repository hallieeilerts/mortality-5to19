
# Code to run a Stan model in parallel and save the results on disc
#require(tidyverse)
#require(rstan)

  # run model
  print(paste("Doing model:", st.input$name))
  ptm0 = proc.time()
  stanfit = rstan::sampling(st.input$model,
                            data = st.data,
                            pars = st.input$param,
                            chains = st.input$nchai, 
                            iter = st.input$niter, 
                            warmup = st.input$nwarm,
                            cores = st.input$cores,
                            control = list('adapt_delta' = .9),
                            seed = 1)
  
  print(paste("Simulation finished, now computing summaries..."))
  
  # if in st.input we stated that we only want the summary information from the model then substitute all the Stan output for only the summary table and add "_summary" to the name of the output object
  if(st.input$summary==1){
    st.ouput <- MCMCsummary(stanfit,  probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
    st.input$name <- paste0(st.input$name,"_summary")
  }else{
    st.output = stanfit
  }
  
  ptm1 = proc.time()
  print(paste("Time taken (min):",(ptm1-ptm0)/60))
  #assign(st.name, list(stanfit=stanfit, st.data=st.data))
  assign(st.input$name, list(st.output=st.output, st.data=st.data, st.input=st.input))
  save(list=c(paste(st.input$name)), file=paste0(st.input$patho,"/", st.input$name,".RData"))

 
  
  