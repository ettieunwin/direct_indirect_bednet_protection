library(zoo)
library(dplyr)
source("R/run_model.R")

##################################################################################################
# Run function
run <- function(init_EIR, usage, model_parameters){
  
  # Time of reading data
  time <- 6*365
  
  model <- run_model(model_parameters = model_parameters, 
                     resistance = 0, init_EIR = init_EIR, usage = usage, 
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

fitted_data <- readRDS("outputs/fitted_EIRs_prev_usage_new.RDS")
model_parameters <- read_excel(path="data/parameter_sweep_res.xlsx")

prevs <- as.numeric(rownames(fitted_data))
usages <- as.numeric(colnames(fitted_data))

model_data <- matrix(ncol = length(usages), nrow = length(prevs))

for (i in 1:length(prevs)){
  for(j in 1:length(usages)){
    if (i == 8 & j > 8){
      print("None")
    } else {
      model_data[i,j] <- run(init_EIR = fitted_data[i,j],
                             usage = usages[j],
                             model_parameters = model_parameters[1,])
    }
  }

}

data <- as.data.frame(model_data)
colnames(data) <- usages
rownames(data) <- prevs

saveRDS(data, "outputs/modelled_data_prev_usage_new.RDS")

