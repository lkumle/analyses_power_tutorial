library(reshape2)
library(tidyverse)
load("~/Dropbox/Power/manuscript/BRM/analyses_BRM/run_times.Rdata")

pd <- run_times

pd[,1] <- pd[,1]/100

pd <- as_tibble(pd) %>%
  mutate(
    trials = 1:20) %>%
  melt(id.vars = "trials") %>%
  mutate(
    variable2 = as.numeric(as.character(variable))) %>%
  group_by(variable2) %>%
  summarise(
    mean = mean(value)
  )

ggplot(pd, aes(x = variable2, y = mean)) +
  geom_point() + 
  geom_line() +
  xlab("simulations") + ggplot2::ylab("minutes") +
  theme(axis.line = ggplot2::element_line(color = "black"), # axis line
        panel.background = ggplot2::element_rect(fill = "white"), # background
        legend.title = ggplot2::element_blank(),
        plot.title = element_text(hjust = 0.47, face="bold")) +
  ggtitle("Run Time") 
  # remove legend title
  #labs(linetype = "condition", color = "condition") +

ggsave("run_times.tiff", units="in", width=5, height=4, dpi=800, compression = 'lzw')

# ------ 2---- # 

load("~/Dropbox/Power/manuscript/BRM/analyses_BRM/nsim_simulations.Rdata")

pd <- sim_results[sim_results$effect == "complexity",] %>%
  mutate(
    trials = 1:20) %>%
  melt(id.vars = c("trials","effect"))
  

ggplot(pd, aes(x = variable, y = value)) +
  geom_jitter(width = 0.1) + 

  xlab("simulations") + ggplot2::ylab("power") +
  theme(axis.line = ggplot2::element_line(color = "black"), # axis line
        panel.background = ggplot2::element_rect(fill = "white"), # background
        legend.title = ggplot2::element_blank(),
        plot.title = element_text(hjust = 0.47, face="bold")) +
  ylim(0.65, 1) +
  ggtitle("Variance between Simulations") 

ggsave("variance.tiff", units="in", width=5, height=4, dpi=800, compression = 'lzw')
