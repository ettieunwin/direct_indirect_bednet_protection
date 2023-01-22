# Computes the control prevalence and cases per year
run_model <- function(init_EIR=100, usage=0, t_max = 6*365,
                      no_insecticide=FALSE, fixed_EIR=FALSE,
                      model_parameters, resistance = 0.0){
  
  # Choose the net parameters based on resistance
  resistance = model_parameters$RESISTANCE
  d_ITN0 <- model_parameters$d_ITN0
  r_ITN0 <- model_parameters$r_ITN0
  itn_half_life <-model_parameters$half_life * 365

  if (no_insecticide == TRUE){
    d_ITN0 <- model_parameters$`u d_ITN0`
    r_ITN0 <- model_parameters$`u r_ITN0`
    itn_half_life <-model_parameters$`u half_life` * 365
  }
  
  # define the net coverage. 
  ITN_IRS_on <- 1*365
  
  # define number of interventions
  if (usage == 0 | usage == 0.0){
    num_int = 1
  } else {
    num_int = 2
  }

  if (fixed_EIR == FALSE){
    out <- ICDMM::run_model(model = "odin_model_mass_effect",
                            num_int = num_int,
                            het_brackets = 5,
                            age = c(0, 0.5, 1, 2, 3.5, 5, 7.5, 10, 15, 20, 30, 40, 50, 60),
                            age6 = 2,
                            init_EIR = init_EIR,
                            country = NULL,
                            admin2 = NULL,
                            ITN_IRS_on = ITN_IRS_on,
                            d_ITN0 = d_ITN0,
                            r_ITN0 = r_ITN0,
                            itn_half_life = itn_half_life,
                            itn_cov = usage,
                            time = t_max,
                            tau1 = model_parameters$tau_1,
                            bites_Bed = model_parameters$phiB)
  } else {
    out <- ICDMM::run_model(model = "odin_model_mass_effect_pp",
                            num_int = num_int,
                            het_brackets = 5,
                            age = c(0, 0.5, 1, 2, 3.5, 5, 7.5, 10, 15, 20, 30, 40, 50, 60),
                            age6 = 2,
                            init_EIR = init_EIR,
                            country = NULL,
                            admin2 = NULL,
                            ITN_IRS_on = ITN_IRS_on,
                            d_ITN0 = d_ITN0,
                            r_ITN0 = r_ITN0,
                            itn_half_life = itn_half_life,
                            itn_cov = usage,
                            time = t_max,
                            tau1 = model_parameters$tau_1,
                            bites_Bed = model_parameters$phiB,
                            Iv_fixed =  0.6617514)
  }
  return (out)
}

