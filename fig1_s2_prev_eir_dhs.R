library(ggplot2)
library(cowplot)
library(zoo)
library(stringr)
library(scales)
library(tidyverse)


#-----------------------------------------------------------------------------------------------
# FIGURE ONE:
# Plots how user and non-user prevalence varies over time

data_long <- readRDS(file = "outputs/prev_data.RDS")
data_long$net = "Treated"
data_long_un <-readRDS(file = "outputs/prev_data_untreated.RDS")
data_long_un$net = "Untreated"

data_long <- rbind(data_long, data_long_un)
colours <- c("darkred", "blue")

p1 <- ggplot(data_long, aes(x=time/365, y=Prevalence)) + 
  geom_line(aes(col=net, linetype=type), size=1) +
  xlab('Time (years)') +
  scale_x_continuous(expand=c(0, 0, 0.05,0)) +
  scale_y_continuous(labels=percent_format(accuracy = 2)) +
  scale_colour_manual(name="",
                      values=colours,
                      guide=guide_legend(ncol=1)) +
  scale_linetype_discrete(name="",
                          breaks=c("all", "non_users", "users"),
                          labels=c("All", "Non-users", 
                                   "Users"),
                          guide=guide_legend(ncol=1)) +
  geom_vline(xintercept = 1, size = 1, linetype='longdash') +
  theme_classic(base_size = 17) + theme(legend.position="bottom") 

#-------------------------------------------------------------------------------------------------
# FIGURE TWO:
eir_data_long <- readRDS( file = "outputs/eirs_data.RDS")
eir_data_long$net <- "Treated"
eir_data_long_untreated <- readRDS( file = "outputs/eirs_data_untreated.RDS")
eir_data_long_untreated$net <- "Untreated"

eir_data_long <- rbind(eir_data_long, eir_data_long_untreated)
eir_data_long$net[eir_data_long$model == "pre_ints"] <- "Pre-Intervention"

colours2 <- c("black", "darkred", "blue")
p2 <- ggplot(eir_data_long, aes(x=eirs, y=Prevalence)) +
  geom_line(aes(col=net, linetype=model), size=1) +
  scale_colour_manual(name="",
                        guide=guide_legend(ncol=1),
                      values = colours2) +
  scale_linetype_discrete(name="",
                          breaks=c("pre_ints", "non_users", "users"),
                          labels=c("Pre-Intervention", "Non-users",  
                                   "Users"), 
                          guide=guide_legend(ncol=1)) +
  geom_vline(xintercept = 100, size = 1, linetype='longdash', col='grey') +
  scale_x_log10(expand=c(0, 0, 0.05, 0)) + xlab("Initial EIR") + 
  scale_y_continuous(expand = c(0, 0), labels=percent_format(accuracy = 2)) +
  theme_classic(base_size = 17) + theme(legend.position="bottom")

#-------------------------------------------------------------------------------------------------
# FIGURE THREE
data_cluster <- readRDS("outputs/cluster_data.RDS")
print(range(data_cluster$diff))

tmp = data_cluster %>% filter(diff %in% c(-1.0, 1.0))

data_cluster <- select(data_cluster, "users", "non_users", "prev_factor")
data_cluster_long <- gather(data_cluster, key = 'type', value = 'prev', -prev_factor)
data_cluster_long$prev_factor <- factor(data_cluster_long$prev_factor,
                                        labels = c(paste0('0 < x ', "\u2264", ' 0.1'),
                                                   paste0('0.1 < x ', "\u2264", ' 0.2'),
                                                   paste0('0.2 < x ', "\u2264", ' 0.3'),
                                                   paste0('0.3 < x ', "\u2264", ' 0.4'),
                                                   paste0('0.4 < x ', "\u2264", ' 0.5'),
                                                   paste0('0.5 < x ', "\u2264", ' 0.6'),
                                                   paste0('0.6 < x ', "\u2264", ' 0.7'),
                                                   paste0('0.7 < x ', "\u2264", ' 0.8'),
                                                   '> 0.8'))

p3 <- ggplot(data_cluster_long) + 
  geom_boxplot(aes(prev_factor, prev, fill = type)) + 
  scale_y_continuous(expand = c(0, 0), labels=percent_format()) +
  theme_bw() + xlab('Cluster prevalence') + ylab("RDT prevalence") + 
  scale_fill_discrete(name = "", labels = c("Non-User", "User")) +
  #scale_colour_discrete(name = "", labels = c("Non-User", "User")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1), text = element_text(size=20),
        legend.position = "bottom")
p3


dat = data_cluster_long %>% group_by(prev_factor, type) %>% 
  summarise(median = median(prev))

dat_wide = spread(dat, key = type, value = median)
dat_wide$diff = dat_wide$non_users- dat_wide$users
print(range(dat_wide$diff ))
#-------------------------------------------------------------------------------------------------
# FIGURE FOUR
data_mean <- readRDS("outputs/cluster_prev_usage_mean.RDS")

num_cluster <- as.data.frame(readRDS("outputs/num_cluster_prev_usage.RDS") )
num_cluster$prev <- rownames(data_mean)
num_cluster <- gather(num_cluster, "key" = usage, value = "value", -prev)

data_mean$prev <- rownames(data_mean)
data_mean <- gather(data_mean, "key" = usage, value = "value", -prev)

num_cluster$usage <- data_mean$usage
names(num_cluster) <- c("prev", "usage", "num")

data_mean_join <- left_join(data_mean, num_cluster)

model_data <- readRDS("outputs/modelled_data_prev_usage_new.RDS")
model_data$prev <- rownames(model_data)
model_data_long <- gather(model_data, key = usage, value = diff, -prev)

sc <- scale_colour_gradient2(name = "Difference", low="blue", high="red", limits=c(0, 0.2))
sf <- scale_fill_gradient2(name = "Difference", low="blue", high="red", limits=c(-0.05, 0.2))


p4 <- ggplot(data_mean_join %>% filter(num > 1, usage > 0.2, usage <= 0.8, prev < 0.9)) + 
  geom_tile(data = model_data_long %>% filter(usage > 0.2, usage <= 0.8), 
            aes(x=usage, y = prev, fill = diff)) +
  geom_point(aes(x = usage, y = prev, fill = value, size = num), pch=21) + #, col = "black") + 
  sf +
  scale_size(name = "Number of clusters", range = c(1,10)) +
  xlab("Cluster usage") + ylab("Cluster prevalence") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        text = element_text(size=20)) 

p = cowplot::plot_grid(p1, p2, p4, p3, ncol=2, labels = 'auto') 
#p <- plot_grid(p_top, p4,
#               nrow=2, labels = c("", "C"), rel_heights = c(1, 1.7)) 

print(p)

cowplot::save_plot(paste("figures/fig_1.png"), p, base_width = 15, base_height = 9)
#cowplot::save_plot(paste("figures/fig_1.pdf"), p, base_width = 15, base_height = 9)


p4 <- ggplot(data_mean_join %>% filter(num > 1)) + 
  geom_tile(data = model_data_long, 
            aes(x=usage, y = prev, fill = diff)) +
  geom_point(aes(x = usage, y = prev, fill = value, size = num), pch=21) + #, col = "black") + 
  sf +
  scale_size(name = "Number of clusters", range = c(1,10)) +
  xlab("Cluster usage") + ylab("Cluster prevalence") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        text = element_text(size=20)) 

ggsave(filename = "figures/fig_s2.png", p4, width = 10)
ggsave(filename = "figures/fig_s2.pdf", p4, width = 10)
