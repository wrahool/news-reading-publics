library(ggplot2)
library(tidyverse)

setwd("C:/Users/Subhayan/Google Drive/Annenberg UPenn/0 Dissertation Project/Dissertation/Figures/Chapter2/")

df <- read_csv("internet_penetration_regional_urban.csv")

df %>%
  gather(Type, Penetration, Urban:Rural) -> df_long

ggplot(df_long) +
  geom_vline(xintercept=seq(2016,2018, by = 1), colour="gray90")+
  geom_line(aes(x=Year, y=Penetration, colour=Type), size = 1.1) +
  geom_point(aes(x=Year, y=Penetration, colour=Type), size = 2) +
  theme_bw() +
  ylim(c(0,100)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())+
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=seq(2015, 2019, 1))
