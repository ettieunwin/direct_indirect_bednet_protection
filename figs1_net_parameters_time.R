library(readxl)

params = read_xlsx("data/parameter_sweep_new.xlsx")

i = 1
t  = 0:(365*3 -  1)
ITN_interval = 3 * 365
itn_loss <- log(2)/(params$half_life[i]*365)
ITN_decay = exp(-(t%%ITN_interval)*itn_loss)

# The r,d and s values turn on after ITN_IRS_on and decay accordingly
r <- 0.24
d_ITN <- params$d_ITN0[i]*ITN_decay
r_ITN <- r + (params$r_ITN0[i] - r)*ITN_decay
s_ITN <- 1 - d_ITN - r_ITN

treated_net = data.frame("t" = t,
                         "d"  = d_ITN,
                         "r" = r_ITN,
                         "s" = s_ITN)
treated_net_long = gather(treated_net, key = "param",  value = "value", -t)
treated_net_long$type = "Treated"


#itn_loss <- log(2)/(params$`u half_life`[i]*365)
itn_loss <- log(2)/(params$half_life[i]*365)
ITN_decay = exp(-(t%%ITN_interval)*itn_loss)

# The r,d and s values turn on after ITN_IRS_on and decay accordingly
r <- 0.24
d_ITN <- params$`u d_ITN0`[i]*ITN_decay
r_ITN <- r + (params$`u r_ITN0`[i] - r)*ITN_decay
s_ITN <- 1 - d_ITN - r_ITN

untreated_net = data.frame("t" = t,
                         "d"  = d_ITN,
                         "r" = r_ITN,
                         "s" = s_ITN)
untreated_net_long = gather(untreated_net, key = "param",  value = "value", -t)
untreated_net_long$type = "Untreated"

net = rbind(treated_net_long, untreated_net_long)
net$param <- factor(net$param, levels = c("d", "r", "s"), labels = c("Killed", "Repeated", "Blood fed"))

p <- ggplot(net) + geom_area(aes(t/365, value, fill = param)) + facet_wrap(~type)+
  theme_bw() + ylab("Probable outcome of feeding attempt (%)") + xlab("Time (years)") + 
  scale_y_continuous(expand = c(0, 0),labels = scales::percent) + scale_x_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = c("#A6CEE3", "#FDBF6F", "#FB9A99")) + 
  labs(fill = "") + theme(legend.position="bottom")
print(p)

ggsave(paste0("figures/fig_s1.png"), p, width =8, height = 4)
ggsave(paste0("figures/fig_s1.pdf"), p, width =8, height = 4)
