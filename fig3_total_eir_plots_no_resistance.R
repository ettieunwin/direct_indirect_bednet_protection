library(tidyverse)
library(readxl)

round.choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

load("outputs/model_data_runs_new.RData")

params <- read_xlsx("data/parameter_sweep_new.xlsx")
params$run <- 1:length(params$RESISTANCE)

user_eir <- cbind(params$run, user_eir)
non_user_eir <- cbind(params$run, non_user_eir)

user_eir_1 <- user_eir[1,]
user_eir$resistance <- resistance[1:length(user_eir$varying_low)]
user_eir <- filter(user_eir, resistance == 0.0)
user_eir$resistance <- NULL

non_user_eir_1 <- non_user_eir[1,]
non_user_eir$resistance <- resistance[1:length(non_user_eir$varying_low)]
non_user_eir <- filter(non_user_eir, resistance == 0.0)
non_user_eir$resistance <- NULL

user_eir_long <- gather(user_eir, key = "run", value = "eir", -`params$run`)
user_eir_long <- filter(user_eir_long, ! run %in% c("varying_low_no_insect",
                                               "varying_0.8_no_insect","varying_0.5_no_insect",
                                               "varying_0.1_no_insect", "fixed_low",
                                               "fixed_0.8","fixed_0.5",
                                               "fixed_0.1", "indirect_low", 
                                               "indirect_0.1", "indirect_0.5",
                                               "indirect_0.8", "varying_low_insect" , 
                                               "varying_0.1_insect",
                                               "varying_0.5_insect", "varying_0.8_insect"))
user_eir_long$run <- factor(user_eir_long$run, 
                            levels = rev(c("fixed_low_no_insect", "fixed_0.1_no_insect",
                                           "fixed_0.5_no_insect", "fixed_0.8_no_insect",
                                           "indirect_low_no_insect", "indirect_0.1_no_insect",
                                           "indirect_0.5_no_insect", "indirect_0.8_no_insect",
                                           "fixed_low_insect" , "fixed_0.1_insect",
                                           "fixed_0.5_insect", "fixed_0.8_insect",
                                           "indirect_low_insect", "indirect_0.1_insect", 
                                           "indirect_0.5_insect", "indirect_0.8_insect",
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

user_eir_long$scenario = ifelse(user_eir_long$run %in% c("One user", "10% usage","50% usage","80% usage"), "A",
                                ifelse(user_eir_long$run %in% c("One user ", "10% usage ", "50% usage ", "80% usage "), "B",
                                       ifelse(user_eir_long$run %in% c("One user  ", "10% usage  ", "50% usage  ", "80% usage  "),  "C", 
                                              ifelse(user_eir_long$run %in% c("One user   ", "10% usage   ", "50% usage   ", "80% usage   "),  "D", "E"))))
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
                                        "80% usage    "), "Total", "Indirect"))
user_eir_long$cat = "Net user"

non_user_eir_long <- gather(non_user_eir, key = "run", value = "eir", -`params$run`)
non_user_eir_long <- filter(non_user_eir_long, ! run %in% c("varying_low_no_insect",
                                                            "varying_0.8_no_insect","varying_0.5_no_insect",
                                                            "varying_0.1_no_insect", "fixed_low",
                                                            "fixed_0.8","fixed_0.5",
                                                            "fixed_0.1", "indirect_low", 
                                                            "indirect_0.1", "indirect_0.5",
                                                            "indirect_0.8", "varying_low_insect" , 
                                                            "varying_0.1_insect",
                                                            "varying_0.5_insect", "varying_0.8_insect"))
non_user_eir_long$run <- factor(non_user_eir_long$run, 
                                levels = rev(c("fixed_low_no_insect", "fixed_0.1_no_insect",
                                               "fixed_0.5_no_insect", "fixed_0.8_no_insect",
                                               "indirect_low_no_insect", "indirect_0.1_no_insect",
                                               "indirect_0.5_no_insect", "indirect_0.8_no_insect",
                                               "fixed_low_insect" , "fixed_0.1_insect",
                                               "fixed_0.5_insect", "fixed_0.8_insect",
                                               "indirect_low_insect", "indirect_0.1_insect", 
                                               "indirect_0.5_insect", "indirect_0.8_insect",
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

non_user_eir_long$scenario = ifelse(non_user_eir_long$run %in% c("One user", "10% usage","50% usage","80% usage"), "A",
                                ifelse(non_user_eir_long$run %in% c("One user ", "10% usage ", "50% usage ", "80% usage "), "B",
                                       ifelse(non_user_eir_long$run %in% c("One user  ", "10% usage  ", "50% usage  ", "80% usage  "),  "C", 
                                              ifelse(non_user_eir_long$run %in% c("One user   ", "10% usage   ", "50% usage   ", "80% usage   "),  "D", "E"))))

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
                                            "80% usage    ")  , "Total", "Indirect"))
non_user_eir_long$cat = "Non net user"

eir_data_long <- rbind(user_eir_long, non_user_eir_long)

dat = eir_data_long
dat$usage = ifelse(dat$run %in% dat$run[grep("One user", dat$run)] , 1e-5, 
                   ifelse(dat$run %in% dat$run[grep("10% usage", dat$run)], 0.1, 
                          ifelse(dat$run %in% dat$run[grep("50% usage", dat$run)], 0.5, 0.8)))
dat$contribution = ifelse(dat$cat == "Net user", dat$eir*dat$usage, dat$eir*(1-dat$usage))

dat$scenario = ifelse(dat$run %in% c("One user", "10% usage","50% usage","80% usage"), "A",
                      ifelse(dat$run %in% c("One user ", "10% usage ", "50% usage ", "80% usage "), "B",
                             ifelse(dat$run %in% c("One user  ", "10% usage  ", "50% usage  ", "80% usage  "),  "C", 
                                    ifelse(dat$run %in% c("One user   ", "10% usage   ", "50% usage   ", "80% usage   "),  "D", "E"))))

overall = dat %>% group_by(`params$run`, run, fill, scenario) %>%
  summarise("eir" = sum(contribution))
overall$cat = "Community"

eir_data_long = rbind(eir_data_long, overall)

#--------------------------------------------------------------
# Numbers in text

a <- eir_data_long %>% filter(cat == "Net user", run == "One user")
print(sprintf("eir direct barrier 1 user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 0.01, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Net user", run == "80% usage")
print(sprintf("eir direct barrier 80%% user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 0.01, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "One user")
print(sprintf("eir direct barrier 1 user community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 0.01, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "80% usage")
print(sprintf("eir direct barrier 80%% user community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 0.01, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Net user", run == "One user ")
print(sprintf("eir indirect barrier 1 user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Non net user", run == "One user ")
print(sprintf("eir indirect barrier 1 non-user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "One user ")
print(sprintf("eir indirect barrier 1 user community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))


a <- eir_data_long %>% filter(cat == "Net user", run == "80% usage ")
print(sprintf("eir indirect barrier 80%% user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Non net user", run == "80% usage ")
print(sprintf("eir indirect barrier 80%% non-user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "80% usage ")
print(sprintf("eir indirect barrier 80%% community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Net user", run == "One user  ")
print(sprintf("eir direct insect 1 user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Net user", run == "80% usage  ")
print(sprintf("eir direct insect 80%% user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025),1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "One user  ")
print(sprintf("eir direct insect 1 community :%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "80% usage  ")
print(sprintf("eir direct insect 80%% community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025),1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Net user", run == "One user   ")
print(sprintf("eir indirect insect 1 user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Non net user", run == "One user   ")
print(sprintf("eir indirect insect 1 non-user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "One user   ")
print(sprintf("eir indirect insect 1 community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Net user", run == "80% usage   ")
print(sprintf("eir indirect insect 80%% user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Non net user", run == "80% usage   ")
print(sprintf("eir indirect insect 80%% non-user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "80% usage   ")
print(sprintf("eir indirect insect 80%% community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Net user", run == "One user    ")
print(sprintf("total eir 1 user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Non net user", run == "One user    ")
print(sprintf("total eir 1 non-user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),2), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "One user    ")
print(sprintf("total eir 1 community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),2), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Net user", run == "80% usage    ")
print(sprintf("total eir 80%% user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Non net user", run == "80% usage    ")
print(sprintf("total eir 80%% non-user:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

a <- eir_data_long %>% filter(cat == "Community", run == "80% usage    ")
print(sprintf("total eir 80%% community:%.0f [95%% CI: %.0f - %.0f]", round(quantile(a$eir, probs= 0.5),0), 
              round.choose(quantile(a$eir, probs= 0.025), 1, 0), round.choose(quantile(a$eir, probs=0.975), 1, 1)))

saveRDS(a, "outputs/community_total.RDS")

#--------------------------------------------------------------
# Reformat for plot
eir_data_long$run[grep("One user", eir_data_long$run)] <- "One user"
eir_data_long$run[grep("10% usage", eir_data_long$run)] <- "10% usage"
eir_data_long$run[grep("50% usage", eir_data_long$run)] <- "50% usage"
eir_data_long$run[grep("80% usage", eir_data_long$run)] <- "80% usage"


#-------------------------
# Plots 
scenario_names <- c(
  "A" = "A: Direct benefit of barrier (control - a)",
  "B" = "B: Indirect benefit of barrier (a - b)",
  "C" = "D: Additional direct benefit of insecticide (a - c)",
  "D" = "D: Additional indirect benefit of insecticide (b - d - (a - c))",
  "E" = "E: All (control - d)"
)

p <- ggplot(eir_data_long) + 
  geom_boxplot(aes(eir/100, run, col = cat, fill = fill)) + 
  facet_wrap(~scenario, ncol = 1, labeller = as_labeller(scenario_names)) + 
  scale_x_continuous(minor_breaks = seq(0 , 100/100, 10/100), breaks = seq(0, 100/100, 20/100), labels = scales::percent) +
  ylab("Scenario") + xlab("Relative reduction in EIR") + scale_colour_discrete(name = "User type") + 
  scale_fill_manual(name = "Protection type", values = c("Direct" = "grey", "Indirect" = "yellow", 
                                                         "Total"= "purple")) +  
  theme_bw()
  
print(p)
ggsave("figures/fig_3.pdf", p, height = 6, width = 8)
ggsave("figures/fig_3.png", p, height = 6, width = 8)

overall = dat %>% filter(fill == "Total") %>% group_by(`params$run`, run, fill, usage) %>%
  summarise("eir" = sum(contribution))

com_data = overall %>% filter(fill == "Total") %>% 
  group_by(usage) %>%
  summarise("mean_eir" = mean(eir))

new_data = data.frame("usage" = seq(0, 0.8, by = 0.1),
                      "mean_eir" = rep(NA, length(seq(0, 0.8, by = 0.1))))
model<-lm(mean_eir ~ poly(usage, 2, raw=TRUE), data = com_data)
new_data$mean_eir <- predict(model, newdata = new_data)

com_data

p2 = ggplot(com_data) +
  geom_point(aes(usage, mean_eir)) + 
  geom_line(data = new_data, aes(usage, mean_eir )) + 
  xlab("Community usage") + ylab("EIR") + 
  theme_bw()

