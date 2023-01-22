library(tidyverse)

dat = readRDS("outputs/cluster_categories.RDS")
dat$group = paste(dat$group_prev, dat$group_usage, sep = "-")

p <- ggplot(dat) +
  geom_boxplot(aes(group, diff))
print(p)

dat2 = select(dat, group, users, non_users)
dat2 = gather(dat2, key = type, value = value, -group)
dat2$group = factor(dat2$group, labels = c("Low prevalence - \nLow usage", 
                                           'Low prevalence - \nHigh usage',
                                           "Medium prevalence - \nLow usage", 
                                           "Medium prevalence - \nHigh usage",
                                           "High prevalence - \nLow usage",
                                           "High prevalence - \nHigh usage"))
p2 <- ggplot(dat2) + 
  geom_boxplot(aes(x = group, y = value, fill = type)) +
  theme_bw() + 
  xlab("Group") + ylab("RDT Prevalence") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete(name = "", labels = c("Non-User", "User")) 
print(p2)

ggsave("figures/fig_s3.pdf", width = 10, height = 5)
ggsave("figures/fig_s3.png", width = 10, height = 5)

dat_predict_boot = readRDS("outputs/cluster_categories_predict_boot.RDS")
dat_predict_boot_long = gather(dat_predict_boot, key = run, value = diff, -group_prev, -group_usage)
dat_predict_boot_long$group = paste(dat_predict_boot_long$group_prev, dat_predict_boot_long$group_usage, sep = "-")
dat_predict_boot_long$group = factor(dat_predict_boot_long$group, labels = c("Low prevalence - \nLow usage", 
                                                         'Low prevalence - \nHigh usage',
                                                         "Medium prevalence - \nLow usage", 
                                                         "Medium prevalence - \nHigh usage",
                                                         "High prevalence - \nLow usage",
                                                         "High prevalence - \nHigh usage"))

p5 <- ggplot(dat_predict_boot_long) + 
  geom_boxplot(aes(x = group, y = as.numeric(diff))) +
  scale_y_continuous(labels = scales::percent) + 
  theme_bw() + 
  xlab("Group") + ylab("RDT Prevalence")
print(p5)
ggsave("figures/fig_s4.pdf", width = 10, height = 5)
ggsave("figures/fig_s4.png", width = 10, height = 5)

prev_group = dat_predict_boot_long[dat_predict_boot_long$group_prev == 2,]

tab_dat = dat_predict_boot_long %>% group_by(group_prev, group_usage) %>% 
  summarise(median = median(diff),
            lower = quantile(diff, probs = 0.025),
            upper = quantile(diff, probs = 0.975))
tab_dat$format = sprintf("%.1f%% [%.1f%%-%.1f%%]", tab_dat$median*100, tab_dat$lower*100, tab_dat$upper*100 )

tab_dat_wide = select(tab_dat, group_prev, group_usage, format)
tab_dat_wide = spread(tab_dat_wide, key = group_prev, value = format)
print(tab_dat_wide)
write_csv(tab_dat_wide, file = "outputs/statistical_model_predict.csv")



