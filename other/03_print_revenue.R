library(ggplot2)
library(tidyverse)

setwd("C:/Users/Subhayan/Google Drive/Annenberg UPenn/0 Dissertation Project/Dissertation/Figures/Chapter2/")

df <- read_csv("print_revenue.csv")

df %>%
  gather(Type, Revenue, English:Regional) %>%
  mutate(Year = as.numeric(Year)) -> df_long

ggplot(df_long) +
  geom_vline(xintercept=seq(2014,2019, by = 1), colour="gray90")+
  geom_line(aes(x=Year, y=Revenue, colour=Type), size = 1.1) +
  geom_point(aes(x=Year, y=Revenue, colour=Type), size = 2) +
  theme_bw() +
  ylim(c(0,100)) +
  xlim(c(2014, 2019)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())+
  theme(legend.position = "bottom")
