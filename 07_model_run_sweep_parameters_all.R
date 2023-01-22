library(tidyverse)
library(readxl)
library(zoo)

source('R/one_set_all.R')

params <- read_xlsx("data/parameter_sweep_new.xlsx")

user_eir <- NULL
non_user_eir <- NULL
user_prev <- NULL
non_user_prev <- NULL

start_time <- Sys.time()
for (i in 1:length(params$RESISTANCE)){
  print(i)
  data = generate_model_runs(model_parameters = params[i,])
  user_eir = rbind(user_eir, data[[1]])
  non_user_eir = rbind(non_user_eir, data[[2]])
  user_prev = rbind(user_prev, data[[3]])
  non_user_prev = rbind(non_user_prev, data[[4]])
}

end_time <- Sys.time()
print(end_time - start_time)

resistance = params$RESISTANCE

save(user_eir, non_user_eir, user_prev, non_user_prev, resistance, 
     file = "outputs/model_data_runs_all.RData")
