library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(zoo)
library(matlab)
library(lme4)
library(survey)
library(plyr)
library(zoo)
library(readxl)
source("R/run_model.R")

model_parameters <- read_xlsx("data/parameter_sweep_new.xlsx")[1,]

full <- run_model(model_parameters = model_parameters, usage = 0.5)

prev_data <- data.frame("time" = full$t, "all" = full$prev659, "non_users" = full$prev659_1,
                       "users" = full$prev659_2)

print(sprintf("All pre: %f", full$prev659[1]))
print(sprintf("All: %f", rollmean(full$prev659, k=3*365,fill = NA, align='left')[3*365]))
print(sprintf("Non users: %f", rollmean(full$prev659_1, k=3*365,fill = NA, align='left')[3*365]))
print(sprintf("Users: %f", rollmean(full$prev659_2, k=3*365,fill = NA, align='left')[3*365]))

prev_data_long <- gather(prev_data, key=type, value=Prevalence, -time)
prev_data_long$type <- factor(prev_data_long$type, c("all", "non_users", "users"))

saveRDS(prev_data_long, file = "outputs/prev_data.RDS")

full <- run_model(model_parameters = model_parameters, usage = 0.5, no_insecticide = TRUE)

prev_data <- data.frame("time" = full$t, "all" = full$prev659, "non_users" = full$prev659_1,
                        "users" = full$prev659_2)

print(sprintf("All pre: %f", full$prev659[1]))
print(sprintf("All: %f", rollmean(full$prev659, k=3*365,fill = NA, align='left')[3*365]))
print(sprintf("Non users: %f", rollmean(full$prev659_1, k=3*365,fill = NA, align='left')[3*365]))
print(sprintf("Users: %f", rollmean(full$prev659_2, k=3*365,fill = NA, align='left')[3*365]))

prev_data_long <- gather(prev_data, key=type, value=Prevalence, -time)
prev_data_long$type <- factor(prev_data_long$type, c("all", "non_users", "users"))

saveRDS(prev_data_long, file = "outputs/prev_data_untreated.RDS")