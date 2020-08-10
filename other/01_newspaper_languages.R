library(ggplot2)
library(tidyverse)

setwd("C:/Users/Subhayan/Google Drive/Annenberg UPenn/0 Dissertation Project/Dissertation/Figures/Chapter2/")

df <- read_csv("newspapers_language2.csv")

df$Language <- as_factor(df$Language)

#df$Language <- factor(df$Language, levels = df[order(df$Total),]$Language)

df %>%
  gather(Type, Count, Dailies:Others, factor_key = T) -> df_long

df_long$Type <- relevel(df_long$Type, 'Others')

df_long$Language <- relevel(df_long$Language, 'Other')


positions <- unique(as.character(df_long[order(-df_long$Total),]$Language))

ggplot(df_long) + 
  geom_bar(width = 0.6, aes(fill=Type, y=Count/10000, x=Language), position="stack", stat="identity") +
  scale_x_discrete(limits = c("Hindi", "English", "Marathi", "Gujarati", "Kannada", "Other")) +
  scale_fill_manual(values = c("sienna1", "cornflowerblue")) +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(text = element_text(size=20))
