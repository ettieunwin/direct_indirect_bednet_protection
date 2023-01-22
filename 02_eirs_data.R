library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(zoo)
library(matlab)
library(lme4)
library(survey)
library(plyr)
library(readxl)
source("R/run_model.R")

model_parameters <- read_xlsx("data/parameter_sweep.xlsx")[1,]

# Loop over EIR.  Want to see how pre-intervention prevalence/ user/non-user prevalence varies
EIRS <- logspace(-1, 3, n = 15)
time_point = 3*365
pre_ints <- vector(length=length(EIRS))
users <- vector(length=length(EIRS))
non_users <- vector(length=length(EIRS))

for (e in 1:length(EIRS)){
  full <- run_model(model_parameters = model_parameters, usage = 0.5, init_EIR = EIRS[e])
  
  pre_ints[e] <- full$prev659[100]
  non_users[e] <- rollmean(full$prev659_1, k=3*365, fill = NA, align='right')[time_point]
  users[e] <- rollmean(full$prev659_2, k=3*365, fill = NA, align='right')[time_point]

}

eir_data <- data.frame("eirs" = EIRS, "pre_ints" = pre_ints, "non_users" = non_users,
                       "users" = users)
eir_data_long <- gather(eir_data, key=model, value=Prevalence, -eirs)
eir_data_long$model <- factor(eir_data_long$model, c("pre_ints", "non_users", "users"))

saveRDS(eir_data_long, file = "outputs/eirs_data.RDS")


pre_ints <- vector(length=length(EIRS))
users <- vector(length=length(EIRS))
non_users <- vector(length=length(EIRS))

for (e in 1:length(EIRS)){
  full <- run_model(model_parameters = model_parameters, usage = 0.5, init_EIR = EIRS[e], 
                    no_insecticide = TRUE)
  
  pre_ints[e] <- full$prev659[100]
  non_users[e] <- rollmean(full$prev659_1, k=3*365, fill = NA, align='right')[time_point]
  users[e] <- rollmean(full$prev659_2, k=3*365, fill = NA, align='right')[time_point]
  
}

eir_data <- data.frame("eirs" = EIRS, "pre_ints" = pre_ints, "non_users" = non_users,
                       "users" = users)
eir_data_long <- gather(eir_data, key=model, value=Prevalence, -eirs)
eir_data_long$model <- factor(eir_data_long$model, c("pre_ints", "non_users", "users"))

saveRDS(eir_data_long, file = "outputs/eirs_data_untreated.RDS")