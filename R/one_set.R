# Generates one set of runs from parameter sweep 

source("R/run_model.R")

generate_model_runs <- function(model_parameters){
  control = run_model(usage = 0.0, model_parameters = model_parameters)
  eir_control = control$EIR[,5,5,1] / (control$rel_foi[,5] * control$foi_age[,5])*365
  
  # PP models --------------------------------------------------------------------
  
  fixed_eir_low = run_model(usage = 1e-5, fixed_EIR = TRUE,
                            model_parameters = model_parameters)
  eir_fixed_usage_low_non_user = fixed_eir_low$EIR[,5,5,1] /
    (fixed_eir_low$rel_foi[,5] * fixed_eir_low$foi_age[,5])*365
  eir_fixed_usage_low_user = fixed_eir_low$EIR[,5,5,2] /
    (fixed_eir_low$rel_foi[,5] * fixed_eir_low$foi_age[,5])*365
  
  fixed_eir_0.8 = run_model(usage = 0.8, fixed_EIR = TRUE,
                            model_parameters = model_parameters)
  eir_fixed_0.8_non_user = fixed_eir_0.8$EIR[,5,5,1] /
    (fixed_eir_0.8$rel_foi[,5] * fixed_eir_0.8$foi_age[,5])*365
  eir_fixed_0.8_user = fixed_eir_0.8$EIR[,5,5,2] /
    (fixed_eir_0.8$rel_foi[,5] * fixed_eir_0.8$foi_age[,5])*365
  
  fixed_eir_0.5 = run_model(usage = 0.5, fixed_EIR = TRUE,
                            model_parameters = model_parameters)
  eir_fixed_0.5_non_user = fixed_eir_0.5$EIR[,5,5,1] /
    (fixed_eir_0.5$rel_foi[,5] * fixed_eir_0.5$foi_age[,5])*365
  eir_fixed_0.5_user = fixed_eir_0.5$EIR[,5,5,2] /
    (fixed_eir_0.5$rel_foi[,5] * fixed_eir_0.5$foi_age[,5])*365
  
  fixed_eir_0.1 = run_model(usage = 0.1, fixed_EIR = TRUE,
                            model_parameters = model_parameters)
  eir_fixed_0.1_non_user = fixed_eir_0.1$EIR[,5,5,1] /
    (fixed_eir_0.1$rel_foi[,5] * fixed_eir_0.1$foi_age[,5])*365
  eir_fixed_0.1_user = fixed_eir_0.1$EIR[,5,5,2] /
    (fixed_eir_0.1$rel_foi[,5] * fixed_eir_0.1$foi_age[,5])*365
  
  fixed_eir_no_insect_low = run_model(usage = 1e-5, 
                                      fixed_EIR = TRUE, no_insecticide = TRUE,
                                      model_parameters = model_parameters)
  eir_fixed_no_insect_low_non_user = fixed_eir_no_insect_low$EIR[,5,5,1] /
    (fixed_eir_no_insect_low$rel_foi[,5] * fixed_eir_no_insect_low$foi_age[,5])*365
  eir_fixed_no_insect_low_user = fixed_eir_no_insect_low$EIR[,5,5,2] /
    (fixed_eir_no_insect_low$rel_foi[,5] * fixed_eir_no_insect_low$foi_age[,5])*365
  
  fixed_eir_no_insect_0.8 = run_model(usage = 0.8, 
                                      fixed_EIR = TRUE, no_insecticide = TRUE,
                                      model_parameters = model_parameters)
  eir_fixed_no_insect_0.8_non_user = fixed_eir_no_insect_0.8$EIR[,5,5,1] /
    (fixed_eir_no_insect_0.8$rel_foi[,5] * fixed_eir_no_insect_0.8$foi_age[,5])*365
  eir_fixed_no_insect_0.8_user = fixed_eir_no_insect_0.8$EIR[,5,5,2] /
    (fixed_eir_no_insect_0.8$rel_foi[,5] * fixed_eir_no_insect_0.8$foi_age[,5])*365
  
  fixed_eir_no_insect_0.5 = run_model(usage = 0.5, 
                                      fixed_EIR = TRUE, no_insecticide = TRUE,
                                      model_parameters = model_parameters)
  eir_fixed_no_insect_0.5_non_user = fixed_eir_no_insect_0.5$EIR[,5,5,1] /
    (fixed_eir_no_insect_0.5$rel_foi[,5] * fixed_eir_no_insect_0.5$foi_age[,5])*365
  eir_fixed_no_insect_0.5_user = fixed_eir_no_insect_0.5$EIR[,5,5,2] /
    (fixed_eir_no_insect_0.5$rel_foi[,5] * fixed_eir_no_insect_0.5$foi_age[,5])*365
  
  fixed_eir_no_insect_0.1 = run_model(usage = 0.1,
                                      fixed_EIR = TRUE, no_insecticide = TRUE,
                                      model_parameters = model_parameters)
  eir_fixed_no_insect_0.1_non_user = fixed_eir_no_insect_0.1$EIR[,5,5,1] /
    (fixed_eir_no_insect_0.1$rel_foi[,5] * fixed_eir_no_insect_0.1$foi_age[,5])*365
  eir_fixed_no_insect_0.1_user = fixed_eir_no_insect_0.1$EIR[,5,5,2] /
    (fixed_eir_no_insect_0.1$rel_foi[,5] * fixed_eir_no_insect_0.1$foi_age[,5])*365
  
  
  
  
  # Full models --------------------------------------------------------------------
  varying_eir_low = run_model(usage = 1e-5, model_parameters = model_parameters)
  eir_varying_usage_low_non_user = varying_eir_low$EIR[,5,5,1] /
    (varying_eir_low$rel_foi[,5] * varying_eir_low$foi_age[,5])*365
  eir_varying_usage_low_user = varying_eir_low$EIR[,5,5,2] /
    (varying_eir_low$rel_foi[,5] * varying_eir_low$foi_age[,5])*365
  
  varying_eir_0.8 = run_model(usage = 0.8, model_parameters = model_parameters)
  eir_varying_0.8_non_user = varying_eir_0.8$EIR[,5,5,1] /
    (varying_eir_0.8$rel_foi[,5] * varying_eir_0.8$foi_age[,5])*365
  eir_varying_0.8_user = varying_eir_0.8$EIR[,5,5,2] /
    (varying_eir_0.8$rel_foi[,5] * varying_eir_0.8$foi_age[,5])*365
  
  varying_eir_0.5 = run_model(usage = 0.5, model_parameters = model_parameters)
  eir_varying_0.5_non_user = varying_eir_0.5$EIR[,5,5,1] /
    (varying_eir_0.5$rel_foi[,5] * varying_eir_0.5$foi_age[,5])*365
  eir_varying_0.5_user = varying_eir_0.5$EIR[,5,5,2] /
    (varying_eir_0.5$rel_foi[,5] * varying_eir_0.5$foi_age[,5])*365
  
  varying_eir_0.1 = run_model(usage = 0.1, model_parameters = model_parameters)
  eir_varying_0.1_non_user = varying_eir_0.1$EIR[,5,5,1] /
    (varying_eir_0.1$rel_foi[,5] * varying_eir_0.1$foi_age[,5])*365
  eir_varying_0.1_user = varying_eir_0.1$EIR[,5,5,2] /
    (varying_eir_0.1$rel_foi[,5] * varying_eir_0.1$foi_age[,5])*365
  
  varying_eir_no_insect_low = run_model(usage = 1e-5,
                                        no_insecticide = TRUE, 
                                        model_parameters = model_parameters)
  eir_varying_no_insect_low_non_user = varying_eir_no_insect_low$EIR[,5,5,1] /
    (varying_eir_no_insect_low$rel_foi[,5] * varying_eir_no_insect_low$foi_age[,5])*365
  eir_varying_no_insect_low_user = varying_eir_no_insect_low$EIR[,5,5,2] /
    (varying_eir_no_insect_low$rel_foi[,5] * varying_eir_no_insect_low$foi_age[,5])*365
  
  varying_eir_no_insect_0.8 = run_model(usage = 0.8,
                                        no_insecticide = TRUE, 
                                        model_parameters = model_parameters)
  eir_varying_no_insect_0.8_non_user = varying_eir_no_insect_0.8$EIR[,5,5,1] /
    (varying_eir_no_insect_0.8$rel_foi[,5] * varying_eir_no_insect_0.8$foi_age[,5])*365
  eir_varying_no_insect_0.8_user = varying_eir_no_insect_0.8$EIR[,5,5,2] /
    (varying_eir_no_insect_0.8$rel_foi[,5] * varying_eir_no_insect_0.8$foi_age[,5])*365
  
  varying_eir_no_insect_0.5 = run_model(usage = 0.5, 
                                        no_insecticide = TRUE,
                                        model_parameters = model_parameters)
  eir_varying_no_insect_0.5_non_user = varying_eir_no_insect_0.5$EIR[,5,5,1] /
    (varying_eir_no_insect_0.5$rel_foi[,5] * varying_eir_no_insect_0.5$foi_age[,5])*365
  eir_varying_no_insect_0.5_user = varying_eir_no_insect_0.5$EIR[,5,5,2] /
    (varying_eir_no_insect_0.5$rel_foi[,5] * varying_eir_no_insect_0.5$foi_age[,5])*365
  
  varying_eir_no_insect_0.1 = run_model(usage = 0.1,  
                                        no_insecticide = TRUE, 
                                        model_parameters = model_parameters)
  eir_varying_no_insect_0.1_non_user = varying_eir_no_insect_0.1$EIR[,5,5,1] /
    (varying_eir_no_insect_0.1$rel_foi[,5] * varying_eir_no_insect_0.1$foi_age[,5])*365
  eir_varying_no_insect_0.1_user = varying_eir_no_insect_0.1$EIR[,5,5,2] /
    (varying_eir_no_insect_0.1$rel_foi[,5] * varying_eir_no_insect_0.1$foi_age[,5])*365
  
  
  
  
  time = 3*365
  control_time = rollmean(eir_control, k=3*365,fill = NA, align='left')[time]
  
  # Calculate reduction in user eir
  user_eir = data.frame(varying_low = control_time - rollmean(eir_varying_usage_low_user, 
                                                              k=3*365,fill = NA, align='left')[time],
                        varying_0.8 = control_time - rollmean(eir_varying_0.8_user, 
                                                              k=3*365,fill = NA, align='left')[time],
                        varying_0.5 = control_time - rollmean(eir_varying_0.5_user, 
                                                              k=3*365,fill = NA, align='left')[time],
                        varying_0.1 = control_time - rollmean(eir_varying_0.1_user, 
                                                              k=3*365,fill = NA, align='left')[time],
                        varying_low_no_insect = control_time - rollmean(eir_varying_no_insect_low_user, 
                                                                        k=3*365,fill = NA, align='left')[time],
                        varying_0.8_no_insect = control_time - rollmean(eir_varying_no_insect_0.8_user, 
                                                                        k=3*365,fill = NA, align='left')[time],
                        varying_0.5_no_insect = control_time - rollmean(eir_varying_no_insect_0.5_user, 
                                                                        k=3*365,fill = NA, align='left')[time],
                        varying_0.1_no_insect = control_time - rollmean(eir_varying_no_insect_0.1_user, 
                                                                        k=3*365,fill = NA, align='left')[time],
                        fixed_low = control_time - rollmean(eir_fixed_usage_low_user, 
                                                            k=3*365,fill = NA, align='left')[time],
                        fixed_0.8 = control_time - rollmean(eir_fixed_0.8_user, 
                                                            k=3*365,fill = NA, align='left')[time],
                        fixed_0.5 = control_time - rollmean(eir_fixed_0.5_user, 
                                                            k=3*365,fill = NA, align='left')[time],
                        fixed_0.1 = control_time - rollmean(eir_fixed_0.1_user, 
                                                            k=3*365,fill = NA, align='left')[time],
                        fixed_low_no_insect = control_time - rollmean(eir_fixed_no_insect_low_user, 
                                                                      k=3*365,fill = NA, align='left')[time],
                        fixed_0.8_no_insect = control_time - rollmean(eir_fixed_no_insect_0.8_user, 
                                                                      k=3*365,fill = NA, align='left')[time],
                        fixed_0.5_no_insect = control_time - rollmean(eir_fixed_no_insect_0.5_user, 
                                                                      k=3*365,fill = NA, align='left')[time],
                        fixed_0.1_no_insect = control_time - rollmean(eir_fixed_no_insect_0.1_user, 
                                                                      k=3*365,fill = NA, align='left')[time])
  
  # Calculate reduction in non-user eir
  non_user_eir = data.frame(varying_low = control_time - rollmean(eir_varying_usage_low_non_user, 
                                                                  k=3*365,fill = NA, align='left')[time],
                            varying_0.8 = control_time - rollmean(eir_varying_0.8_non_user, 
                                                                  k=3*365,fill = NA, align='left')[time],
                            varying_0.5 = control_time - rollmean(eir_varying_0.5_non_user, 
                                                                  k=3*365,fill = NA, align='left')[time],
                            varying_0.1 = control_time - rollmean(eir_varying_0.1_non_user, 
                                                                  k=3*365,fill = NA, align='left')[time],
                            varying_low_no_insect = control_time - rollmean(eir_varying_no_insect_low_non_user, 
                                                                            k=3*365,fill = NA, align='left')[time],
                            varying_0.8_no_insect = control_time - rollmean(eir_varying_no_insect_0.8_non_user, 
                                                                            k=3*365,fill = NA, align='left')[time],
                            varying_0.5_no_insect = control_time - rollmean(eir_varying_no_insect_0.5_non_user, 
                                                                            k=3*365,fill = NA, align='left')[time],
                            varying_0.1_no_insect = control_time - rollmean(eir_varying_no_insect_0.1_non_user, 
                                                                            k=3*365,fill = NA, align='left')[time],
                            fixed_low = control_time - rollmean(eir_fixed_usage_low_non_user, 
                                                                k=3*365,fill = NA, align='left')[time],
                            fixed_0.8 = control_time - rollmean(eir_fixed_0.8_non_user, 
                                                                k=3*365,fill = NA, align='left')[time],
                            fixed_0.5 = control_time - rollmean(eir_fixed_0.5_non_user, 
                                                                k=3*365,fill = NA, align='left')[time],
                            fixed_0.1 = control_time - rollmean(eir_fixed_0.1_non_user, 
                                                                k=3*365,fill = NA, align='left')[time],
                            fixed_low_no_insect = control_time - rollmean(eir_fixed_no_insect_low_non_user, 
                                                                          k=3*365,fill = NA, align='left')[time],
                            fixed_0.8_no_insect = control_time - rollmean(eir_fixed_no_insect_0.8_non_user, 
                                                                          k=3*365,fill = NA, align='left')[time],
                            fixed_0.5_no_insect = control_time - rollmean(eir_fixed_no_insect_0.5_non_user, 
                                                                          k=3*365,fill = NA, align='left')[time],
                            fixed_0.1_no_insect = control_time - rollmean(eir_fixed_no_insect_0.1_non_user, 
                                                                          k=3*365,fill = NA, align='left')[time])
  
  
  
  # Calculate reduction in user prev
  control_prev_time = rollmean(control$prev_a, k=3*365,fill = NA, align='left')[time]
  user_prev = data.frame(varying_low = control_prev_time - rollmean(varying_eir_low$prev_a_2, 
                                                                    k=3*365,fill = NA, align='left')[time],
                         varying_0.8 = control_prev_time - rollmean(varying_eir_0.8$prev_a_2, 
                                                                    k=3*365,fill = NA, align='left')[time],
                         varying_0.5 = control_prev_time - rollmean(varying_eir_0.5$prev_a_2, 
                                                                    k=3*365,fill = NA, align='left')[time],
                         varying_0.1 = control_prev_time - rollmean(varying_eir_0.1$prev_a_2, 
                                                                    k=3*365,fill = NA, align='left')[time],
                         varying_low_no_insect = control_prev_time - rollmean(varying_eir_no_insect_low$prev_a_2, 
                                                                              k=3*365,fill = NA, align='left')[time],
                         varying_0.8_no_insect = control_prev_time - rollmean(varying_eir_no_insect_0.8$prev_a_2, 
                                                                              k=3*365,fill = NA, align='left')[time],
                         varying_0.5_no_insect = control_prev_time - rollmean(varying_eir_no_insect_0.5$prev_a_2, 
                                                                              k=3*365,fill = NA, align='left')[time],
                         varying_0.1_no_insect = control_prev_time - rollmean(varying_eir_no_insect_0.1$prev_a_2, 
                                                                              k=3*365,fill = NA, align='left')[time],
                         fixed_low = control_prev_time - rollmean(fixed_eir_low$prev_a_2, 
                                                                  k=3*365,fill = NA, align='left')[time],
                         fixed_0.8 = control_prev_time - rollmean(fixed_eir_0.8$prev_a_2, 
                                                                  k=3*365,fill = NA, align='left')[time],
                         fixed_0.5 = control_prev_time - rollmean(fixed_eir_0.5$prev_a_2, 
                                                                  k=3*365,fill = NA, align='left')[time],
                         fixed_0.1 = control_prev_time - rollmean(fixed_eir_0.1$prev_a_2, 
                                                                  k=3*365,fill = NA, align='left')[time],
                         fixed_low_no_insect = control_prev_time - rollmean(fixed_eir_no_insect_low$prev_a_2, 
                                                                            k=3*365,fill = NA, align='left')[time],
                         fixed_0.8_no_insect = control_prev_time - rollmean(fixed_eir_no_insect_0.8$prev_a_2, 
                                                                            k=3*365,fill = NA, align='left')[time],
                         fixed_0.5_no_insect = control_prev_time - rollmean(fixed_eir_no_insect_0.5$prev_a_2, 
                                                                            k=3*365,fill = NA, align='left')[time],
                         fixed_0.1_no_insect = control_prev_time - rollmean(fixed_eir_no_insect_0.1$prev_a_2, 
                                                                            k=3*365,fill = NA, align='left')[time])
  # Calculate reduction in non user prev
  non_user_prev = data.frame(varying_low = control_prev_time - rollmean(varying_eir_low$prev_a_1, 
                                                                        k=3*365,fill = NA, align='left')[time],
                             varying_0.8 = control_prev_time - rollmean(varying_eir_0.8$prev_a_1, 
                                                                        k=3*365,fill = NA, align='left')[time],
                             varying_0.5 = control_prev_time - rollmean(varying_eir_0.5$prev_a_1, 
                                                                        k=3*365,fill = NA, align='left')[time],
                             varying_0.1 = control_prev_time - rollmean(varying_eir_0.1$prev_a_1, 
                                                                        k=3*365,fill = NA, align='left')[time],
                             varying_low_no_insect = control_prev_time - rollmean(varying_eir_no_insect_low$prev_a_1, 
                                                                                  k=3*365,fill = NA, align='left')[time],
                             varying_0.8_no_insect = control_prev_time - rollmean(varying_eir_no_insect_0.8$prev_a_1, 
                                                                                  k=3*365,fill = NA, align='left')[time],
                             varying_0.5_no_insect = control_prev_time - rollmean(varying_eir_no_insect_0.5$prev_a_1, 
                                                                                  k=3*365,fill = NA, align='left')[time],
                             varying_0.1_no_insect = control_prev_time - rollmean(varying_eir_no_insect_0.1$prev_a_1, 
                                                                                  k=3*365,fill = NA, align='left')[time],
                             fixed_low = control_prev_time - rollmean(fixed_eir_low$prev_a_1, 
                                                                      k=3*365,fill = NA, align='left')[time],
                             fixed_0.8 = control_prev_time - rollmean(fixed_eir_0.8$prev_a_1, 
                                                                      k=3*365,fill = NA, align='left')[time],
                             fixed_0.5 = control_prev_time - rollmean(fixed_eir_0.5$prev_a_1, 
                                                                      k=3*365,fill = NA, align='left')[time],
                             fixed_0.1 = control_prev_time - rollmean(fixed_eir_0.1$prev_a_1, 
                                                                      k=3*365,fill = NA, align='left')[time],
                             fixed_low_no_insect = control_prev_time - rollmean(fixed_eir_no_insect_low$prev_a_1, 
                                                                                k=3*365,fill = NA, align='left')[time],
                             fixed_0.8_no_insect = control_prev_time - rollmean(fixed_eir_no_insect_0.8$prev_a_1, 
                                                                                k=3*365,fill = NA, align='left')[time],
                             fixed_0.5_no_insect = control_prev_time - rollmean(fixed_eir_no_insect_0.5$prev_a_1, 
                                                                                k=3*365,fill = NA, align='left')[time],
                             fixed_0.1_no_insect = control_prev_time - rollmean(fixed_eir_no_insect_0.1$prev_a_1, 
                                                                                k=3*365,fill = NA, align='left')[time])
  
  # Calculate indirect only
  user_eir$indirect_low = user_eir$varying_low - user_eir$fixed_low
  user_eir$indirect_low_no_insect = user_eir$varying_low_no_insect - user_eir$fixed_low_no_insect
  user_eir$indirect_0.1 = user_eir$varying_0.1 - user_eir$fixed_0.1
  user_eir$indirect_0.1_no_insect = user_eir$varying_0.1_no_insect - user_eir$fixed_0.1_no_insect
  user_eir$indirect_0.5 = user_eir$varying_0.5 - user_eir$fixed_0.5
  user_eir$indirect_0.5_no_insect = user_eir$varying_0.5_no_insect - user_eir$fixed_0.5_no_insect
  user_eir$indirect_0.8 = user_eir$varying_0.8 - user_eir$fixed_0.8
  user_eir$indirect_0.8_no_insect = user_eir$varying_0.8_no_insect - user_eir$fixed_0.8_no_insect
  
  non_user_eir$indirect_low = non_user_eir$varying_low - non_user_eir$fixed_low
  non_user_eir$indirect_low_no_insect = non_user_eir$varying_low_no_insect - non_user_eir$fixed_low_no_insect
  non_user_eir$indirect_0.1 = non_user_eir$varying_0.1 - non_user_eir$fixed_0.1
  non_user_eir$indirect_0.1_no_insect = non_user_eir$varying_0.1_no_insect - non_user_eir$fixed_0.1_no_insect
  non_user_eir$indirect_0.5 = non_user_eir$varying_0.5 - non_user_eir$fixed_0.5
  non_user_eir$indirect_0.5_no_insect = non_user_eir$varying_0.5_no_insect - non_user_eir$fixed_0.5_no_insect
  non_user_eir$indirect_0.8 = non_user_eir$varying_0.8 - non_user_eir$fixed_0.8
  non_user_eir$indirect_0.8_no_insect = non_user_eir$varying_0.8_no_insect - non_user_eir$fixed_0.8_no_insect
  
  user_prev$indirect_low = user_prev$varying_low - user_prev$fixed_low
  user_prev$indirect_low_no_insect = user_prev$varying_low_no_insect - user_prev$fixed_low_no_insect
  user_prev$indirect_0.1 = user_prev$varying_0.1 - user_prev$fixed_0.1
  user_prev$indirect_0.1_no_insect = user_prev$varying_0.1_no_insect - user_prev$fixed_0.1_no_insect
  user_prev$indirect_0.5 = user_prev$varying_0.5 - user_prev$fixed_0.5
  user_prev$indirect_0.5_no_insect = user_prev$varying_0.5_no_insect - user_prev$fixed_0.5_no_insect
  user_prev$indirect_0.8 = user_prev$varying_0.8 - user_prev$fixed_0.8
  user_prev$indirect_0.8_no_insect = user_prev$varying_0.8_no_insect - user_prev$fixed_0.8_no_insect
  
  non_user_prev$indirect_low = non_user_prev$varying_low - non_user_prev$fixed_low
  non_user_prev$indirect_low_no_insect = non_user_prev$varying_low_no_insect - non_user_prev$fixed_low_no_insect
  non_user_prev$indirect_0.1 = non_user_prev$varying_0.1 - non_user_prev$fixed_0.1
  non_user_prev$indirect_0.1_no_insect = non_user_prev$varying_0.1_no_insect - non_user_prev$fixed_0.1_no_insect
  non_user_prev$indirect_0.5 = non_user_prev$varying_0.5 - non_user_prev$fixed_0.5
  non_user_prev$indirect_0.5_no_insect = non_user_prev$varying_0.5_no_insect - non_user_prev$fixed_0.5_no_insect
  non_user_prev$indirect_0.8 = non_user_prev$varying_0.8 - non_user_prev$fixed_0.8
  non_user_prev$indirect_0.8_no_insect = non_user_prev$varying_0.8_no_insect - non_user_prev$fixed_0.8_no_insect

  # Calculate insecticide only
  user_eir$fixed_low_insect = user_eir$fixed_low - user_eir$fixed_low_no_insect
  user_eir$varying_low_insect = user_eir$varying_low - user_eir$varying_low_no_insect
  user_eir$indirect_low_insect = user_eir$indirect_low - user_eir$indirect_low_no_insect
  user_eir$fixed_0.1_insect = user_eir$fixed_0.1 - user_eir$fixed_0.1_no_insect
  user_eir$varying_0.1_insect = user_eir$varying_0.1 - user_eir$varying_0.1_no_insect
  user_eir$indirect_0.1_insect = user_eir$indirect_0.1 - user_eir$indirect_0.1_no_insect
  user_eir$fixed_0.5_insect = user_eir$fixed_0.5 - user_eir$fixed_0.5_no_insect
  user_eir$varying_0.5_insect = user_eir$varying_0.5 - user_eir$varying_0.5_no_insect
  user_eir$indirect_0.5_insect = user_eir$indirect_0.5 - user_eir$indirect_0.5_no_insect
  user_eir$fixed_0.8_insect = user_eir$fixed_0.8 - user_eir$fixed_0.8_no_insect
  user_eir$varying_0.8_insect = user_eir$varying_0.8 - user_eir$varying_0.8_no_insect
  user_eir$indirect_0.8_insect = user_eir$indirect_0.8 - user_eir$indirect_0.8_no_insect
  
  non_user_eir$fixed_low_insect = non_user_eir$fixed_low - non_user_eir$fixed_low_no_insect
  non_user_eir$varying_low_insect = non_user_eir$varying_low - non_user_eir$varying_low_no_insect
  non_user_eir$indirect_low_insect = non_user_eir$indirect_low - non_user_eir$indirect_low_no_insect
  non_user_eir$fixed_0.1_insect = non_user_eir$fixed_0.1 - non_user_eir$fixed_0.1_no_insect
  non_user_eir$varying_0.1_insect = non_user_eir$varying_0.1 - non_user_eir$varying_0.1_no_insect
  non_user_eir$indirect_0.1_insect = non_user_eir$indirect_0.1 - non_user_eir$indirect_0.1_no_insect
  non_user_eir$fixed_0.5_insect = non_user_eir$fixed_0.5 - non_user_eir$fixed_0.5_no_insect
  non_user_eir$varying_0.5_insect = non_user_eir$varying_0.5 - non_user_eir$varying_0.5_no_insect
  non_user_eir$indirect_0.5_insect = non_user_eir$indirect_0.5 - non_user_eir$indirect_0.5_no_insect
  non_user_eir$fixed_0.8_insect = non_user_eir$fixed_0.8 - non_user_eir$fixed_0.8_no_insect
  non_user_eir$varying_0.8_insect = non_user_eir$varying_0.8 - non_user_eir$varying_0.8_no_insect
  non_user_eir$indirect_0.8_insect = non_user_eir$indirect_0.8 - non_user_eir$indirect_0.8_no_insect
  
  user_prev$fixed_low_insect = user_prev$fixed_low - user_prev$fixed_low_no_insect
  user_prev$varying_low_insect = user_prev$varying_low - user_prev$varying_low_no_insect
  user_prev$indirect_low_insect = user_prev$indirect_low - user_prev$indirect_low_no_insect
  user_prev$fixed_0.1_insect = user_prev$fixed_0.1 - user_prev$fixed_0.1_no_insect
  user_prev$varying_0.1_insect = user_prev$varying_0.1 - user_prev$varying_0.1_no_insect
  user_prev$indirect_0.1_insect = user_prev$indirect_0.1 - user_prev$indirect_0.1_no_insect
  user_prev$fixed_0.5_insect = user_prev$fixed_0.5 - user_prev$fixed_0.5_no_insect
  user_prev$varying_0.5_insect = user_prev$varying_0.5 - user_prev$varying_0.5_no_insect
  user_prev$indirect_0.5_insect = user_prev$indirect_0.5 - user_prev$indirect_0.5_no_insect
  user_prev$fixed_0.8_insect = user_prev$fixed_0.8 - user_prev$fixed_0.8_no_insect
  user_prev$varying_0.8_insect = user_prev$varying_0.8 - user_prev$varying_0.8_no_insect
  user_prev$indirect_0.8_insect = user_prev$indirect_0.8 - user_prev$indirect_0.8_no_insect
  
  non_user_prev$fixed_low_insect = non_user_prev$fixed_low - non_user_prev$fixed_low_no_insect
  non_user_prev$varying_low_insect = non_user_prev$varying_low - non_user_prev$varying_low_no_insect
  non_user_prev$indirect_low_insect = non_user_prev$indirect_low - non_user_prev$indirect_low_no_insect
  non_user_prev$fixed_0.1_insect = non_user_prev$fixed_0.1 - non_user_prev$fixed_0.1_no_insect
  non_user_prev$varying_0.1_insect = non_user_prev$varying_0.1 - non_user_prev$varying_0.1_no_insect
  non_user_prev$indirect_0.1_insect = non_user_prev$indirect_0.1 - non_user_prev$indirect_0.1_no_insect
  non_user_prev$fixed_0.5_insect = non_user_prev$fixed_0.5 - non_user_prev$fixed_0.5_no_insect
  non_user_prev$varying_0.5_insect = non_user_prev$varying_0.5 - non_user_prev$varying_0.5_no_insect
  non_user_prev$indirect_0.5_insect = non_user_prev$indirect_0.5 - non_user_prev$indirect_0.5_no_insect
  non_user_prev$fixed_0.8_insect = non_user_prev$fixed_0.8 - non_user_prev$fixed_0.8_no_insect
  non_user_prev$varying_0.8_insect = non_user_prev$varying_0.8 - non_user_prev$varying_0.8_no_insect
  non_user_prev$indirect_0.8_insect = non_user_prev$indirect_0.8 - non_user_prev$indirect_0.8_no_insect

  
  return(list(user_eir, non_user_eir, user_prev, non_user_prev))
}