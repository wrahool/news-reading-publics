library(ggplot2)
library(tidyverse)

setwd("C:/Users/Subhayan/Google Drive/Annenberg UPenn/0 Dissertation Project/Dissertation/Figures/Chapter2/")

df <- read_csv("internet_penetration.csv")

df %>%
  gather(Year, Percentage, `1990`:`2017`) %>%
  mutate(Year = as.numeric(Year)) -> df_long

ggplot(df_long) +
  geom_line(aes(x=Year, y=Percentage, colour=Country), size = 1.1) +
  ylim(c(0, 100)) +
  xlim(c(1990, 2020)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=20))
