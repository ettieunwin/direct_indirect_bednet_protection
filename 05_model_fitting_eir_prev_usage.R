library(zoo)
library(tidyverse)
library(readxl)
source("R/run_model.R")

##################################################################################################
# Run function
run <- function(init_EIR, usage, model_parameters){
  # Time of reading data
  time <- 6*365
  
  model <- run_model(model_parameters = model_parameters, 
                     resistance = 0, init_EIR = init_EIR, usage = usage, 
                     t_max=10*365)  
  prev <- rollmean(model$prev659, k=3*365, fill = NA, align='left')[time]
  print(prev)
  return (prev)
}


# Fitting method
bisection_fitting <- function(usage, prevalence, model_parameters){

  n <- 100
  
  # Set initial conditions and start the algorithm
  if (prevalence > 0.6 & usage > 0.4){
    init_EIRs <- c(100, 9000)
  }  else if (prevalence > 0.6 & usage <= 0.4){
    init_EIRs <- c(100, 7000)
  } else {
    init_EIRs <- c(0.1, 2000)
  }
  run_lower <- run(init_EIR = init_EIRs[1], usage = usage, model_parameters = model_parameters)
  run_upper <- run(init_EIR = init_EIRs[2], usage = usage, model_parameters = model_parameters)
  run_values <- c(run_lower - prevalence, run_upper - prevalence)
  
  print (run_values)
  if (sign(run_values[1]) == sign(run_values[2])){
    stop("Incorrect initial range of EIRs")
  }
  
  for (i in 1:n){
    
    print(i)
    
    new_EIR <- sum(init_EIRs) / 2
    
    # Check convergence
    if ((init_EIRs[2] - init_EIRs[1])/2 < 1e-2){
      print ("Converged!")
      return (new_EIR)
    }
    
    new_value <-  run(init_EIR = new_EIR, usage = usage, model_parameters = model_parameters) - 
      prevalence
    
    print(prevalence)
    if (sign(new_value) == sign(run_values[1])){
      init_EIRs[1] <- new_EIR
      run_values[1] <- new_value
    } else {
      init_EIRs[2] <- new_EIR
      run_values[2] <- new_value
    }
  }

  print ("Not converged and reached iteration count.")
  return (NULL)
}


model_parameters <- read_excel(path="data/parameter_sweep_res.xlsx")

prevs <- c(1:8/10)
usages <- c(1:8/10)
fitted_EIRs <- matrix(nrow = length(prevs), ncol = length(usages))


for (i in 1:length(prevs)){
  for (j in 1:length(usages)){
    fitted_EIRs[i, j] <- bisection_fitting(usage = usages[j], 
                                           prevalence = prevs[i], 
                                           model_parameters = model_parameters[1,])
  }
}

data <- data.frame(fitted_EIRs)
colnames(data) = usages
rownames(data) = prevs

saveRDS(data, "outputs/fitted_EIRs_prev_usage_new.RDS")
