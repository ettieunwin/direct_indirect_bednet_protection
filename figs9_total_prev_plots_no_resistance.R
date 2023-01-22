library(tidyverse)
library(readxl)
load("outputs/model_data_runs_new.RData")

params <- read_xlsx("data/parameter_sweep_new.xlsx")
params$run <- 1:length(params$RESISTANCE)

user_prev <- cbind(params$run, user_prev)
non_user_prev <- cbind(params$run, non_user_prev)

user_prev_1 <- user_prev[1,]
user_prev$resistance <- resistance[1:length(user_prev$varying_low)]
user_prev <- filter(user_prev, resistance == 0.0)
user_prev$resistance <- NULL

non_user_prev_1 <- non_user_prev[1,]
non_user_prev$resistance <- resistance[1:length(non_user_prev$varying_low)]
non_user_prev <- filter(non_user_prev, resistance == 0.0)
non_user_prev$resistance <- NULL

user_prev_long <- gather(user_prev, key = "run", value = "prev", -`params$run`)
user_prev_long <- filter(user_prev_long, ! run %in% c("varying_low_no_insect",
                                               "varying_0.8_no_insect","varying_0.5_no_insect",
                                               "varying_0.1_no_insect", "fixed_low",
                                               "fixed_0.8","fixed_0.5",
                                               "fixed_0.1", "indirect_low", 
                                               "indirect_0.1", "indirect_0.5",
                                               "indirect_0.8", "varying_low_insect" , 
                                               "varying_0.1_insect",
                                               "varying_0.5_insect", "varying_0.8_insect"))
user_prev_long$run <- factor(user_prev_long$run, 
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
user_prev_long$fill <- ifelse(user_prev_long$run %in% 
                               c("One user",
                                 "10% usage",
                                 "50% usage",
                                 "80% usage",
                                 "One user  ",
                                 "10% usage  ",
                                 "50% usage  ",
                                 "80% usage  "), "Direct", 
                             ifelse(user_prev_long$run %in% 
                                      c("One user    ",
                                        "10% usage    ",
                                        "50% usage    ",
                                        "80% usage    "), "Total", "Indirect"))
user_prev_long$cat = "Net user"

user_prev_long$scenario = ifelse(user_prev_long$run %in% c("One user", "10% usage","50% usage","80% usage"), "A",
                                ifelse(user_prev_long$run %in% c("One user ", "10% usage ", "50% usage ", "80% usage "), "B",
                                       ifelse(user_prev_long$run %in% c("One user  ", "10% usage  ", "50% usage  ", "80% usage  "),  "C", 
                                              ifelse(user_prev_long$run %in% c("One user   ", "10% usage   ", "50% usage   ", "80% usage   "),  "D", "E"))))



non_user_prev_long <- gather(non_user_prev, key = "run", value = "prev", -`params$run`)
non_user_prev_long <- filter(non_user_prev_long, ! run %in% c("varying_low_no_insect",
                                                            "varying_0.8_no_insect","varying_0.5_no_insect",
                                                            "varying_0.1_no_insect", "fixed_low",
                                                            "fixed_0.8","fixed_0.5",
                                                            "fixed_0.1", "indirect_low", 
                                                            "indirect_0.1", "indirect_0.5",
                                                            "indirect_0.8", "varying_low_insect" , 
                                                            "varying_0.1_insect",
                                                            "varying_0.5_insect", "varying_0.8_insect"))
non_user_prev_long$run <- factor(non_user_prev_long$run, 
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

non_user_prev_long$fill <- ifelse(non_user_prev_long$run %in% 
                                   c("One user",
                                     "10% usage",
                                     "50% usage",
                                     "80% usage",
                                     "One user  ",
                                     "10% usage  ",
                                     "50% usage  ",
                                     "80% usage  "), "Direct", 
                                 ifelse(user_prev_long$run %in% 
                                          c("One user    ",
                                            "10% usage    ",
                                            "50% usage    ",
                                            "80% usage    ")  , "Total", "Indirect"))
non_user_prev_long$cat = "Non net user"

non_user_prev_long$scenario = ifelse(non_user_prev_long$run %in% c("One user", "10% usage","50% usage","80% usage"), "A",
                                    ifelse(non_user_prev_long$run %in% c("One user ", "10% usage ", "50% usage ", "80% usage "), "B",
                                           ifelse(non_user_prev_long$run %in% c("One user  ", "10% usage  ", "50% usage  ", "80% usage  "),  "C", 
                                                  ifelse(non_user_prev_long$run %in% c("One user   ", "10% usage   ", "50% usage   ", "80% usage   "),  "D", "E"))))


prev_data_long <- rbind(user_prev_long, non_user_prev_long)

dat = prev_data_long
dat$usage = ifelse(dat$run %in% dat$run[grep("One user", dat$run)] , 1e-5, 
                   ifelse(dat$run %in% dat$run[grep("10% usage", dat$run)], 0.1, 
                          ifelse(dat$run %in% dat$run[grep("50% usage", dat$run)], 0.5, 0.8)))
dat$contribution = ifelse(dat$cat == "Net user", dat$prev*dat$usage, dat$prev*(1-dat$usage))

overall = dat %>% group_by(`params$run`, run, fill, scenario) %>%
  summarise("prev" = sum(contribution))
overall$cat = "Community"

prev_data_long = rbind(prev_data_long, overall)

# Reformat for plot
prev_data_long$run[grep("One user", prev_data_long$run)] <- "One user"
prev_data_long$run[grep("10% usage", prev_data_long$run)] <- "10% usage"
prev_data_long$run[grep("50% usage", prev_data_long$run)] <- "50% usage"
prev_data_long$run[grep("80% usage", prev_data_long$run)] <- "80% usage"


#-------------------------

scenario_names <- c(
  "A" = "A: Direct benefit of barrier (control - a)",
  "B" = "B: Indirect benefit of barrier (a - b)",
  "C" = "C: Additional direct benefit of insecticide (a - c)",
  "D" = "D: Additional indirect benefit of insecticide (b - d - (a - c))",
  "E" = "E: All (control - d)"
)

p <- ggplot(prev_data_long) + 
  geom_boxplot(aes(prev/0.4779928, run, col = cat, fill = fill)) + 
  facet_wrap(~scenario, ncol = 1, labeller = as_labeller(scenario_names)) + 
  scale_x_continuous(labels = scales::percent) + 
  ylab("Scenario") + xlab("Relative reduction in prevalence") + scale_colour_discrete(name = "User type") + 
  scale_fill_manual(name = "Protection type", values = c("Direct" = "grey", "Indirect" = "yellow", 
                                                         "Total"= "purple")) +  
  theme_bw()
  
print(p)
ggsave("figures/fig_s9.pdf", p, height = 6, width = 8)
ggsave("figures/fig_s9.png", p, height = 6, width = 8)
