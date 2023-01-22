library(lme4)
library(glmmTMB)
library(sjPlot)
library(lmerTest)
library(languageR)
library(lmerTest)

admin_data <- readRDS("outputs/admin_level_usage_prev.RDS")
admin_data_sub <- admin_data[which(!is.nan(admin_data$resistance)),]
admin_data_sub$resistance = as.numeric(admin_data_sub$resistance)
admin_data_sub$diff <- admin_data_sub$user_prev - admin_data_sub$non_user_prev

admin_data_sub$time <- ifelse(admin_data_sub$year <= 2014, 1, 2)
admin_data_sub$year <- factor(admin_data_sub$year)
m <- lmer(diff ~ (1 |country) + time , 
          data = admin_data_sub)
plot_model(m)
plot_model(m, "re")
print(summary(m))
anova(m)

m2 <- lmer(diff ~ (1|country) + resistance, data = admin_data_sub)
plot_model(m2)
plot_model(m2, "re")
summary(m2)
anova(m2)
