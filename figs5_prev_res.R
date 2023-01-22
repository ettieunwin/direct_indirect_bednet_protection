library(ggplot2)
library(cowplot)
library(zoo)
library(stringr)
library(scales)


model_data <- readRDS("outputs/modelled_data_prev_res.RDS")
model_data$prev <- rownames(model_data)
model_data_long <- gather(model_data, key = res, value = diff, -prev)

sc <- scale_colour_gradient2(name = "Difference", low="blue", high="red", limits=c(0, 0.2))
sf <- scale_fill_gradient2(name = "Difference", low="blue", high="red", limits=c(0, 0.2))

p4 <- ggplot(model_data_long ) + 
  geom_tile(aes(x=res, y = prev, fill = diff)) +
  sf +
  xlab("Resistance") + ylab("Prevalence") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        text = element_text(size=20)) 
p4


print(p4)
ggsave(paste("figures/fig_s5.png"), p4, width = 6, height = 5)
ggsave(paste("figures/fig_s5.pdf"), p4, width = 6, height = 5)
