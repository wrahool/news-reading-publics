library(ggplot2)
library(tidyverse)

setwd("C:/Users/Subhayan/Google Drive/Annenberg UPenn/0 Dissertation Project/Dissertation/Figures/Chapter2/")

df <- read_csv("newspapers_language.csv")

df$Language <- as_factor(df$Language)

df %>%
  gather(Type, Count, Dailies:Others, factor_key = T) -> df_long

df_long$Type <- relevel(df_long$Type, 'Others')

df_long$Language <- factor(df_long$Language, levels = df_long[order(df_long$Total),]$Language)

positions <- unique(as.character(df_long[order(-df_long$Total),]$Language))

ggplot(df_long) + 
  geom_bar(aes(fill=Type, y=Count/10000, x=Language), position="stack", stat="identity") +
  scale_x_discrete(limits = positions) +
  scale_fill_manual(values = c("sienna1", "cornflowerblue")) +
  coord_flip() +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_blank())
