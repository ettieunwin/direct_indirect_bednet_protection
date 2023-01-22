library(tidyverse)
library(readxl)

load("outputs/model_data_runs_new.RData")
params <- read_xlsx("data/parameter_sweep_new.xlsx")
params$run <- 1:length(params$RESISTANCE)

user_eir <- cbind(params$run, user_eir)
non_user_eir <- cbind(params$run, non_user_eir)

user_eir$resistance <- resistance[1:length(user_eir$varying_low)]

non_user_eir$resistance <- resistance[1:length(non_user_eir$varying_low)]

user_eir_long <- gather(user_eir, key = "run", value = "eir", -resistance, -`params$run`)
user_eir_long <- filter(user_eir_long, ! run %in% c("fixed_low",
                                                    "fixed_0.8","fixed_0.5",
                                                    "fixed_0.1", "indirect_low", "indirect_0.8",
                                                    "indirect_0.5", "indirect_0.1", "indirect_low_insect",
                                                    "indirect_0.8_insect","indirect_0.5_insect",
                                                    "indirect_0.1_insect", "indirect_low_no_insect", 
                                                    "indirect_0.1_no_insect", "indirect_0.5_no_insect",
                                                    "indirect_0.8_no_insect"))
user_eir_long$run <- factor(user_eir_long$run, 
                            levels = rev(c("fixed_low_no_insect", "fixed_0.1_no_insect",
                                           "fixed_0.5_no_insect", "fixed_0.8_no_insect",
                                           "varying_low_no_insect", "varying_0.1_no_insect",
                                           "varying_0.5_no_insect", "varying_0.8_no_insect",
                                           "fixed_low_insect" , "fixed_0.1_insect",
                                           "fixed_0.5_insect", "fixed_0.8_insect",
                                           "varying_low_insect", "varying_0.1_insect", 
                                           "varying_0.5_insect", "varying_0.8_insect",
                                           "varying_low", "varying_0.1", "varying_0.5",            
                                           "varying_0.8")), 
                            labels = rev(c("One user",
                                           "10% usage",
                                           "50% usage",
                                           "80% usage",
                                           "One user ",
                                           "10% usage ",
                                           "50% usage ",
                                           "80% usage ",
                                           "One user  ",
                                           "10% usage  ",
                                           "50% usage  ",
                                           "80% usage  ",
                                           "One user   ",
                                           "10% usage   ",
                                           "50% usage   ",
                                           "80% usage   ",
                                           "One user    ",
                                           "10% usage    ",
                                           "50% usage    ",
                                           "80% usage    ")))
user_eir_long$fill <- ifelse(user_eir_long$run %in% 
                               c("One user",
                                 "10% usage",
                                 "50% usage",
                                 "80% usage",
                                 "One user  ",
                                 "10% usage  ",
                                 "50% usage  ",
                                 "80% usage  "), "Direct", 
                             ifelse(user_eir_long$run %in% 
                                      c("One user    ",
                                        "10% usage    ",
                                        "50% usage    ",
                                        "80% usage    "), "Total", "Direct + Indirect"))

user_eir_long$cat = "Net user"

user_eir_long$scenario = ifelse(user_eir_long$run %in% c("One user", "10% usage","50% usage","80% usage"), "A",
                                ifelse(user_eir_long$run %in% c("One user ", "10% usage ", "50% usage ", "80% usage "), "B",
                                       ifelse(user_eir_long$run %in% c("One user  ", "10% usage  ", "50% usage  ", "80% usage  "),  "C", 
                                              ifelse(user_eir_long$run %in% c("One user   ", "10% usage   ", "50% usage   ", "80% usage   "),  "D", "E"))))


non_user_eir_long <- gather(non_user_eir, key = "run", value = "eir", -resistance, -`params$run`)
non_user_eir_long <- filter(non_user_eir_long, ! run %in% c("fixed_low",
                                                            "fixed_0.8","fixed_0.5",
                                                            "fixed_0.1", "indirect_low", "indirect_0.8",
                                                            "indirect_0.5", "indirect_0.1", "indirect_low_insect",
                                                            "indirect_0.8_insect","indirect_0.5_insect",
                                                            "indirect_0.1_insect", "indirect_low_no_insect", 
                                                            "indirect_0.1_no_insect", "indirect_0.5_no_insect",
                                                            "indirect_0.8_no_insect"))
non_user_eir_long$run <- factor(non_user_eir_long$run, 
                            levels = rev(c("fixed_low_no_insect", "fixed_0.1_no_insect",
                                           "fixed_0.5_no_insect", "fixed_0.8_no_insect",
                                           "varying_low_no_insect", "varying_0.1_no_insect",
                                           "varying_0.5_no_insect", "varying_0.8_no_insect",
                                           "fixed_low_insect" , "fixed_0.1_insect",
                                           "fixed_0.5_insect", "fixed_0.8_insect",
                                           "varying_low_insect", "varying_0.1_insect", 
                                           "varying_0.5_insect", "varying_0.8_insect",
                                           "varying_low", "varying_0.1", "varying_0.5",            
                                           "varying_0.8")), 
                            labels = rev(c("One user",
                                           "10% usage",
                                           "50% usage",
                                           "80% usage",
                                           "One user ",
                                           "10% usage ",
                                           "50% usage ",
                                           "80% usage ",
                                           "One user  ",
                                           "10% usage  ",
                                           "50% usage  ",
                                           "80% usage  ",
                                           "One user   ",
                                           "10% usage   ",
                                           "50% usage   ",
                                           "80% usage   ",
                                           "One user    ",
                                           "10% usage    ",
                                           "50% usage    ",
                                           "80% usage    ")))
non_user_eir_long$fill <- ifelse(non_user_eir_long$run %in% 
                                   c("One user",
                                     "10% usage",
                                     "50% usage",
                                     "80% usage",
                                     "One user  ",
                                     "10% usage  ",
                                     "50% usage  ",
                                     "80% usage  "), "Direct", 
                                 ifelse(user_eir_long$run %in% 
                                          c("One user    ",
                                            "10% usage    ",
                                            "50% usage    ",
                                            "80% usage    "), "Total", "Direct + Indirect"))

non_user_eir_long$cat = "Non net user"

non_user_eir_long$scenario = ifelse(non_user_eir_long$run %in% c("One user", "10% usage","50% usage","80% usage"), "A",
                                    ifelse(non_user_eir_long$run %in% c("One user ", "10% usage ", "50% usage ", "80% usage "), "B",
                                           ifelse(non_user_eir_long$run %in% c("One user  ", "10% usage  ", "50% usage  ", "80% usage  "),  "C", 
                                                  ifelse(non_user_eir_long$run %in% c("One user   ", "10% usage   ", "50% usage   ", "80% usage   "),  "D", "E"))))



eir_data_long <- rbind(user_eir_long, non_user_eir_long)
eir_data_long$resistance <- factor(eir_data_long$resistance, labels = c('0%', '40%', '80%'))

dat = eir_data_long
dat$usage = ifelse(dat$run %in% dat$run[grep("One user", dat$run)] , 1e-5, 
                   ifelse(dat$run %in% dat$run[grep("10% usage", dat$run)], 0.1, 
                          ifelse(dat$run %in% dat$run[grep("50% usage", dat$run)], 0.5, 0.8)))
dat$contribution = ifelse(dat$cat == "Net user", dat$eir*dat$usage, dat$eir*(1-dat$usage))

dat$scenario = ifelse(dat$run %in% c("One user", "10% usage","50% usage","80% usage"), "A",
                      ifelse(dat$run %in% c("One user ", "10% usage ", "50% usage ", "80% usage "), "B",
                             ifelse(dat$run %in% c("One user  ", "10% usage  ", "50% usage  ", "80% usage  "),  "C", 
                                    ifelse(dat$run %in% c("One user   ", "10% usage   ", "50% usage   ", "80% usage   "),  "D", "E"))))

overall = dat %>% group_by(`params$run`, run, fill, scenario, resistance) %>%
  summarise("eir" = sum(contribution))
overall$cat = "Community"

eir_data_long = rbind(eir_data_long, overall)

a <- eir_data_long %>% filter(cat == "Community",
                              resistance == "0%", 
                              run == "50% usage ")
print(sprintf("eir direct + indirect barrier 50%% community 0 resistance:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

com_tot = readRDS("outputs/community_total_no_res.RDS")
com_dat = left_join(a, com_tot, by = c("params$run", "cat"))
com_dat$percentage = (1 - com_dat$eir.x/com_dat$eir.y) *100
print(sprintf("percentage 80%% community 0%% resistance:%.0f [95%% CI: %.0f - %.0f]", round(quantile(com_dat$percentage, probs= 0.5),0), 
              round.choose(quantile(com_dat$percentage, probs= 0.025), 1, 0), round.choose(quantile(com_dat$percentage, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community",
                              resistance == "80%", 
                              run == "50% usage ")
print(sprintf("eir direct + indirect barrier 50%% community 80%% resistance:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

com_tot = readRDS("outputs/community_total_res.RDS")
com_dat = left_join(a, com_tot, by = c("params$run", "cat"))
com_dat$percentage = (1 - com_dat$eir.x/com_dat$eir.y) *100
print(sprintf("percentage 80%% community 0%% resistance:%.0f [95%% CI: %.0f - %.0f]", round(quantile(com_dat$percentage, probs= 0.5),0), 
              round.choose(quantile(com_dat$percentage, probs= 0.025), 1, 0), round.choose(quantile(com_dat$percentage, probs=0.975), 1, 1)))


eir_data_long$run[grep("One user", eir_data_long$run)] <- "One user"
eir_data_long$run[grep("10% usage", eir_data_long$run)] <- "10% usage"
eir_data_long$run[grep("50% usage", eir_data_long$run)] <- "50% usage"
eir_data_long$run[grep("80% usage", eir_data_long$run)] <- "80% usage"

scenario_names <- c(
  "C" = "A: Direct (a - c)",
  "D" = "B: Direct + Indirect (b - d)",
  "E" = "C: All (control - d)"
)

p <- ggplot(eir_data_long %>% 
         filter(scenario %in% c("C", "D", "E"))) + 
  geom_boxplot(aes(eir/100, run, col = resistance, fill = fill)) + 
  facet_grid(scenario~cat, labeller = labeller(scenario = scenario_names)) + 
  scale_x_continuous(minor_breaks = seq(0 , 1, 10/100), breaks = seq(0, 1, 20/100), labels = scales::percent) +
  scale_fill_manual(name = "Protection type", values = c("Direct" = "grey", "Direct + Indirect" = "pink1", "Total"= "purple")) + 
  xlab("Relative reduction in EIR") + ylab("Scenario") + scale_colour_discrete(name = "Resistance") + 
  theme_bw()

  
print(p)
ggsave("figures/fig_s10.pdf", p, height = 7, width = 12)
ggsave("figures/fig_s10.png", p, height = 7, width = 12)
