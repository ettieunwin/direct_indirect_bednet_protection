library(zoo)
library(dplyr)
library(readxl)
source("R/run_model.R")

##################################################################################################
# Run function
run <- function(init_EIR, model_parameters){
  
  # Time of reading data
  time <- 6*365
  
  model <- run_model(usage = 0.5, model_parameters = model_parameters, 
                     resistance = 0, init_EIR = init_EIR, 
                     t_max=10*365)  
  # Prevs in kids
  prev <- rollmean(model$prev, k=3*365, fill = NA, align='left')[time]
  non_user_prev <- rollmean(model$prev1, k=3*365, fill = NA, align='left')[time]
  user_prev <- rollmean(model$prev2, k=3*365, fill = NA, align='left')[time]
  
  prev_data <- data.frame("overall" = prev,
                          "user" = user_prev,
                          "non_user" = non_user_prev)
  return (prev_data$non_user - prev_data$user)
}
##################################################################################################

fitted_data <- readRDS("outputs/fitted_EIRs_prev_resistance.RDS")
model_parameters =  read_excel(path="data/parameter_sweep_res.xlsx")

prevs <- as.numeric(rownames(fitted_data))
res <- as.numeric(colnames(fitted_data))

model_data <- matrix(ncol = length(res), nrow = length(prevs))

for (i in 1:length(prevs)){
  for(j in 1:length(res)){
    model_data[i,j] <- run(init_EIR = fitted_data[i,j],
                           model_parameters = model_parameters[j,])
  }
}

data <- as.data.frame(model_data)
colnames(data) <- res
rownames(data) <- prevs

saveRDS(data, "outputs/modelled_data_prev_res.RDS")

