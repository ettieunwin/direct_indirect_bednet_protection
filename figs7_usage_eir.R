library(tidyverse)
library(readxl)

load("outputs/model_data_runs_all.RData")

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
                                                    "varying_1.0_no_insect","varying_0.9_no_insect",
                                                    "varying_0.8_no_insect","varying_0.7_no_insect",
                                                    "varying_0.6_no_insect","varying_0.5_no_insect",
                                                    "varying_0.4_no_insect","varying_0.3_no_insect",
                                                    "varying_0.2_no_insect","varying_0.1_no_insect", 
                                                    "fixed_low",
                                                    "fixed_1.0","fixed_0.9",
                                                    "fixed_0.8","fixed_0.7",
                                                    "fixed_0.6","fixed_0.5",
                                                    "fixed_0.4","fixed_0.3",
                                                    "fixed_0.2","fixed_0.1", 
                                                    "indirect_low", 
                                                    "indirect_1.0", "indirect_0.9",
                                                    "indirect_0.8", "indirect_0.7",
                                                    "indirect_0.6", "indirect_0.5",
                                                    "indirect_0.4", "indirect_0.3",
                                                    "indirect_0.2", "indirect_0.1", 
                                                    "varying_low_insect" , 
                                                    "varying_1.0_insect","varying_0.9_insect",
                                                    "varying_0.8_insect","varying_0.7_insect",
                                                    "varying_0.6_insect", "varying_0.5_insect",
                                                    "varying_0.4_insect","varying_0.3_insect",
                                                    "varying_0.2_insect","varying_0.1_insect"))
user_eir_long$run <- factor(user_eir_long$run, 
                            levels = rev(c("fixed_low_no_insect", "fixed_0.1_no_insect",
                                           "fixed_0.2_no_insect", "fixed_0.3_no_insect",
                                           "fixed_0.4_no_insect", "fixed_0.5_no_insect", 
                                           "fixed_0.6_no_insect","fixed_0.7_no_insect",
                                           "fixed_0.8_no_insect","fixed_0.9_no_insect",
                                           "fixed_1.0_no_insect",
                                           "indirect_low_no_insect", "indirect_0.1_no_insect",
                                           "indirect_0.2_no_insect", "indirect_0.3_no_insect",
                                           "indirect_0.4_no_insect", "indirect_0.5_no_insect", 
                                           "indirect_0.6_no_insect", "indirect_0.7_no_insect",
                                           "indirect_0.8_no_insect", "indirect_0.9_no_insect",
                                           "indirect_1.0_no_insect",
                                           "fixed_low_insect", "fixed_0.1_insect",
                                           "fixed_0.2_insect", "fixed_0.3_insect",
                                           "fixed_0.4_insect", "fixed_0.5_insect", 
                                           "fixed_0.6_insect", "fixed_0.7_insect",
                                           "fixed_0.8_insect", "fixed_0.9_insect",
                                           "fixed_1.0_insect",
                                           "indirect_low_insect", "indirect_0.1_insect", 
                                           "indirect_0.2_insect", "indirect_0.3_insect", 
                                           "indirect_0.4_insect", "indirect_0.5_insect", 
                                           "indirect_0.6_insect", "indirect_0.7_insect", 
                                           "indirect_0.8_insect", "indirect_0.9_insect", 
                                           "indirect_1.0_insect", 
                                           "varying_low", "varying_0.1", 
                                           "varying_0.2", "varying_0.3", 
                                           "varying_0.4", "varying_0.5", 
                                           "varying_0.6", "varying_0.7", 
                                           "varying_0.8", "varying_0.9",
                                           "varying_1.0")), 
                            labels = rev(c("One user",
                                           "10% usage",
                                           "20% usage",
                                           "30% usage",
                                           "40% usage",
                                           "50% usage",
                                           "60% usage",
                                           "70% usage",
                                           "80% usage",
                                           "90% usage",
                                           "100% usage",
                                           "One user ",
                                           "10% usage ",
                                           "20% usage ",
                                           "30% usage ",
                                           "40% usage ",
                                           "50% usage ",
                                           "60% usage ",
                                           "70% usage ",
                                           "80% usage ",
                                           "90% usage ",
                                           "100% usage ",
                                           "One user  ",
                                           "10% usage  ",
                                           "20% usage  ",
                                           "30% usage  ",
                                           "40% usage  ",
                                           "50% usage  ",
                                           "60% usage  ",
                                           "70% usage  ",
                                           "80% usage  ",
                                           "90% usage  ",
                                           "100% usage  ",
                                           "One user   ",
                                           "10% usage   ",
                                           "20% usage   ",
                                           "30% usage   ",
                                           "40% usage   ",
                                           "50% usage   ",
                                           "60% usage   ",
                                           "70% usage   ",
                                           "80% usage   ",
                                           "90% usage   ",
                                           "100% usage   ",
                                           "One user    ",
                                           "10% usage    ",
                                           "20% usage    ",
                                           "30% usage    ",
                                           "40% usage    ",
                                           "50% usage    ",
                                           "60% usage    ",
                                           "70% usage    ",
                                           "80% usage    ",
                                           "90% usage    ",
                                           "100% usage    ")))

user_eir_long$scenario = ifelse(user_eir_long$run %in% c("One user", "10% usage", "20% usage", "30% usage", 
                                                         "40% usage","50% usage", "60% usage", "70% usage", 
                                                         "80% usage", "90% usage", "100% usage"), "A",
                                ifelse(user_eir_long$run %in% c("One user ", "10% usage ", "20% usage ", 
                                                                "30% usage ", "40% usage ", "50% usage ", 
                                                                "60% usage ", "70% usage ", "80% usage ",
                                                                "90% usage ", "100% usage "), "B",
                                       ifelse(user_eir_long$run %in% c("One user  ", "10% usage  ", 
                                                                       "20% usage  ", "30% usage  ",
                                                                       "40% usage  ", "50% usage  ", 
                                                                       "60% usage  ", "70% usage  ",
                                                                       "80% usage  ", "90% usage  ",
                                                                       "109% usage  "),  "C", 
                                              ifelse(user_eir_long$run %in% c("One user   ", "10% usage   ", 
                                                                              "20% usage   ", "30% usage   ",
                                                                              "40% usage   ", "50% usage   ",
                                                                              "60% usage   ", "70% usage   ",
                                                                              "80% usage   ", "90% usage   ",
                                                                              "100% usage   "),  "D", "E"))))
user_eir_long$fill <- ifelse(user_eir_long$run %in% 
                               c("One user",
                                 "10% usage",
                                 "20% usage",
                                 "30% usage",
                                 "40% usage",
                                 "50% usage",
                                 "60% usage",
                                 "70% usage",
                                 "80% usage",
                                 "90% usage",
                                 "100% usage",
                                 "One user  ",
                                 "10% usage  ",
                                 "20% usage  ",
                                 "30% usage  ",
                                 "40% usage  ",
                                 "50% usage  ",
                                 "60% usage  ",
                                 "70% usage  ",
                                 "80% usage  ",
                                 "90% usage  ",
                                 "100% usage  "), "Direct", 
                             ifelse(user_eir_long$run %in% 
                                      c("One user    ",
                                        "10% usage    ",
                                        "20% usage    ",
                                        "30% usage    ",
                                        "40% usage    ",
                                        "50% usage    ",
                                        "60% usage    ",
                                        "70% usage    ",
                                        "80% usage    ",
                                        "90% usage    ",
                                        "100% usage    "), "Total", "Indirect"))
user_eir_long$cat = "Net user"

non_user_eir_long <- gather(non_user_eir, key = "run", value = "eir", -`params$run`)
non_user_eir_long <- filter(non_user_eir_long, ! run %in% c("varying_low_no_insect",
                                                            "varying_1.0_no_insect","varying_0.9_no_insect",
                                                            "varying_0.8_no_insect","varying_0.7_no_insect",
                                                            "varying_0.6_no_insect","varying_0.5_no_insect",
                                                            "varying_0.4_no_insect","varying_0.3_no_insect",
                                                            "varying_0.2_no_insect","varying_0.1_no_insect", 
                                                            "fixed_low",
                                                            "fixed_1.0","fixed_0.9",
                                                            "fixed_0.8","fixed_0.7",
                                                            "fixed_0.6","fixed_0.5",
                                                            "fixed_0.4","fixed_0.3",
                                                            "fixed_0.2","fixed_0.1", 
                                                            "indirect_low", 
                                                            "indirect_1.0", "indirect_0.9",
                                                            "indirect_0.8", "indirect_0.7",
                                                            "indirect_0.6", "indirect_0.5",
                                                            "indirect_0.4", "indirect_0.3",
                                                            "indirect_0.2", "indirect_0.1", 
                                                            "varying_low_insect" , 
                                                            "varying_1.0_insect","varying_0.9_insect",
                                                            "varying_0.8_insect","varying_0.7_insect",
                                                            "varying_0.6_insect", "varying_0.5_insect",
                                                            "varying_0.4_insect","varying_0.3_insect",
                                                            "varying_0.2_insect","varying_0.1_insect"))
non_user_eir_long$run <- factor(non_user_eir_long$run, 
                                levels = rev(c("fixed_low_no_insect", "fixed_0.1_no_insect",
                                             "fixed_0.2_no_insect", "fixed_0.3_no_insect",
                                             "fixed_0.4_no_insect", "fixed_0.5_no_insect", 
                                             "fixed_0.6_no_insect","fixed_0.7_no_insect",
                                             "fixed_0.8_no_insect","fixed_0.9_no_insect",
                                             "fixed_1.0_no_insect",
                                             "indirect_low_no_insect", "indirect_0.1_no_insect",
                                             "indirect_0.2_no_insect", "indirect_0.3_no_insect",
                                             "indirect_0.4_no_insect", "indirect_0.5_no_insect", 
                                             "indirect_0.6_no_insect", "indirect_0.7_no_insect",
                                             "indirect_0.8_no_insect", "indirect_0.9_no_insect",
                                             "indirect_1.0_no_insect",
                                             "fixed_low_insect", "fixed_0.1_insect",
                                             "fixed_0.2_insect", "fixed_0.3_insect",
                                             "fixed_0.4_insect", "fixed_0.5_insect", 
                                             "fixed_0.6_insect", "fixed_0.7_insect",
                                             "fixed_0.8_insect", "fixed_0.9_insect",
                                             "fixed_1.0_insect",
                                             "indirect_low_insect", "indirect_0.1_insect", 
                                             "indirect_0.2_insect", "indirect_0.3_insect", 
                                             "indirect_0.4_insect", "indirect_0.5_insect", 
                                             "indirect_0.6_insect", "indirect_0.7_insect", 
                                             "indirect_0.8_insect", "indirect_0.9_insect", 
                                             "indirect_1.0_insect", 
                                             "varying_low", "varying_0.1", 
                                             "varying_0.2", "varying_0.3", 
                                             "varying_0.4", "varying_0.5", 
                                             "varying_0.6", "varying_0.7", 
                                             "varying_0.8", "varying_0.9",
                                             "varying_1.0")), 
                                labels = rev(c("One user",
                                               "10% usage",
                                               "20% usage",
                                               "30% usage",
                                               "40% usage",
                                               "50% usage",
                                               "60% usage",
                                               "70% usage",
                                               "80% usage",
                                               "90% usage",
                                               "100% usage",
                                               "One user ",
                                               "10% usage ",
                                               "20% usage ",
                                               "30% usage ",
                                               "40% usage ",
                                               "50% usage ",
                                               "60% usage ",
                                               "70% usage ",
                                               "80% usage ",
                                               "90% usage ",
                                               "100% usage ",
                                               "One user  ",
                                               "10% usage  ",
                                               "20% usage  ",
                                               "30% usage  ",
                                               "40% usage  ",
                                               "50% usage  ",
                                               "60% usage  ",
                                               "70% usage  ",
                                               "80% usage  ",
                                               "90% usage  ",
                                               "100% usage  ",
                                               "One user   ",
                                               "10% usage   ",
                                               "20% usage   ",
                                               "30% usage   ",
                                               "40% usage   ",
                                               "50% usage   ",
                                               "60% usage   ",
                                               "70% usage   ",
                                               "80% usage   ",
                                               "90% usage   ",
                                               "100% usage   ",
                                               "One user    ",
                                               "10% usage    ",
                                               "20% usage    ",
                                               "30% usage    ",
                                               "40% usage    ",
                                               "50% usage    ",
                                               "60% usage    ",
                                               "70% usage    ",
                                               "80% usage    ",
                                               "90% usage    ",
                                               "100% usage    ")))


non_user_eir_long$scenario = ifelse(non_user_eir_long$run %in% c("One user", "10% usage", "20% usage", "30% usage", 
                                                             "40% usage","50% usage", "60% usage", "70% usage", 
                                                             "80% usage", "90% usage", "100% usage"), "A",
                                    ifelse(non_user_eir_long$run %in% c("One user ", "10% usage ", "20% usage ", 
                                                                    "30% usage ", "40% usage ", "50% usage ", 
                                                                    "60% usage ", "70% usage ", "80% usage ",
                                                                    "90% usage ", "100% usage "), "B",
                                           ifelse(non_user_eir_long$run %in% c("One user  ", "10% usage  ", 
                                                                           "20% usage  ", "30% usage  ",
                                                                           "40% usage  ", "50% usage  ", 
                                                                           "60% usage  ", "70% usage  ",
                                                                           "80% usage  ", "90% usage  ",
                                                                           "109% usage  "),  "C", 
                                                  ifelse(non_user_eir_long$run %in% c("One user   ", "10% usage   ", 
                                                                                  "20% usage   ", "30% usage   ",
                                                                                  "40% usage   ", "50% usage   ",
                                                                                  "60% usage   ", "70% usage   ",
                                                                                  "80% usage   ", "90% usage   ",
                                                                                  "100% usage   "),  "D", "E"))))
non_user_eir_long$fill <- ifelse(non_user_eir_long$run %in% 
                                   c("One user",
                                     "10% usage",
                                     "20% usage",
                                     "30% usage",
                                     "40% usage",
                                     "50% usage",
                                     "60% usage",
                                     "70% usage",
                                     "80% usage",
                                     "90% usage",
                                     "100% usage",
                                     "One user  ",
                                     "10% usage  ",
                                     "20% usage  ",
                                     "30% usage  ",
                                     "40% usage  ",
                                     "50% usage  ",
                                     "60% usage  ",
                                     "70% usage  ",
                                     "80% usage  ",
                                     "90% usage  ",
                                     "100% usage  "), "Direct", 
                                 ifelse(non_user_eir_long$run %in% 
                                          c("One user    ",
                                            "10% usage    ",
                                            "20% usage    ",
                                            "30% usage    ",
                                            "40% usage    ",
                                            "50% usage    ",
                                            "60% usage    ",
                                            "70% usage    ",
                                            "80% usage    ",
                                            "90% usage    ",
                                            "100% usage    "), "Total", "Indirect"))
non_user_eir_long$cat = "Non net user"

eir_data_long <- rbind(user_eir_long, non_user_eir_long)

dat = eir_data_long
dat$usage = ifelse(dat$run %in% dat$run[grep("One user", dat$run)] , 1e-5, 
                   ifelse(dat$run %in% dat$run[grep("10% usage", dat$run)], 0.1, 
                          ifelse(dat$run %in% dat$run[grep("20% usage", dat$run)], 0.2, 
                                 ifelse(dat$run %in% dat$run[grep("30% usage", dat$run)], 0.3, 
                                        ifelse(dat$run %in% dat$run[grep("40% usage", dat$run)], 0.4,
                                               ifelse(dat$run %in% dat$run[grep("50% usage", dat$run)], 0.5, 
                                                      ifelse(dat$run %in% dat$run[grep("60% usage", dat$run)], 0.6,
                                                             ifelse(dat$run %in% dat$run[grep("70% usage", dat$run)], 0.7,
                                                                    ifelse(dat$run %in% dat$run[grep("80% usage", dat$run)], 0.8,
                                                                           ifelse(dat$run %in% dat$run[grep("90% usage", dat$run)], 0.9,
                                                                                  0.9999))))))))))
dat$contribution = ifelse(dat$cat == "Net user", dat$eir*dat$usage, dat$eir*(1-dat$usage))

# dat$scenario = ifelse(dat$run %in% c("One user", "10% usage", "20% usage", "30% usage", "40% usage",
#                                      "50% usage","60% usage", "70% usage", "80% usage", "90% usage",
#                                      "100% usage",), "A",
#                       ifelse(dat$run %in% c("One user ", "10% usage ", "20% usage ", "30% usage ",
#                                             "40% usage ","50% usage ", "10% usage ", "10% usage ",
#                                             "80% usage ", "90% usage ", "100% usage "), "B",
#                              ifelse(dat$run %in% c("One user  ", "10% usage  ", "20% usage  ",
#                                                    "30% usage  ", "40% usage  ", "50% usage  ", 
#                                                    "60% usage  ", "70% usage  ", "80% usage  ",
#                                                    "90% usage  ", "100% usage  "),  "C", 
#                                     ifelse(dat$run %in% c("One user   ", "10% usage   ",
#                                                           "20% usage   ", "30% usage   ",
#                                                           "40% usage   ", "50% usage   ", 
#                                                           "60% usage   ", "70% usage   ",
#                                                           "80% usage   ", "90% usage   ",
#                                                           "100% usage   "),  "D", "E"))))


overall = dat %>% filter(fill == "Total") %>% group_by(`params$run`, run, fill, usage) %>%
  summarise("eir" = sum(contribution)) %>%
  ungroup()

com_data = overall %>% 
  group_by(usage) %>%
  summarise("median_eir" = median(eir)) %>%
  ungroup()

new_data = data.frame("usage" = seq(0, 1.0, by = 0.1),
                      "median_eir" = rep(NA, length(seq(0, 1.0, by = 0.1))))
model<-lm(median_eir ~ poly(usage, 3, raw=TRUE), data = com_data)
new_data$median_eir <- predict(model, newdata = new_data)

overall$usage = as.integer(overall$usage * 10)
p1 = ggplot(overall %>% select(usage, eir)) +
  geom_boxplot(aes(usage/10, eir/100, group = usage)) + 
  geom_line(data = new_data, aes(usage, median_eir/100)) + 
  scale_x_continuous(minor_breaks = seq(0 , 100/100, 10/100), breaks = seq(0, 100/100, 20/100), labels = scales::percent) +
  scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.2), labels = scales::percent) +
  xlab("Community usage") + ylab("Cumulative relative reduction in EIR") + 
  theme_bw()

print(p1)

ggsave("figures/fig_s7.pdf")
ggsave("figures/fig_s7.png")
