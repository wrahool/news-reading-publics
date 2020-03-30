rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(tidyverse)

df <- read_csv("../../../0 Dissertation Project/Dissertation/Useful/barc_data_reshaped.csv")

df %>%
  filter(!Language %in% c("BHOJPURI","MULTI LANGUAGE")) %>%
  mutate(week = str_replace(week, "W", "")) %>%
  mutate(week = str_replace(week, "_2018", "")) %>%
  mutate(week = as.numeric(week)) %>%
  mutate(week = week - 13) -> plot_df

ggplot(plot_df, aes(x=week, y=impressions*1000 / 1000000)) +
  geom_line(aes(color=Language), size=1) +
  geom_point(aes(color=Language)) +
  scale_color_brewer(palette="Paired") +
  #scale_color_manual(values=my_palette) +
  scale_x_continuous(breaks=seq(1, 13, 1), minor_breaks = NULL) +
  theme_bw()

library(randomcoloR)
n <- 9
my_palette <- distinctColorPalette(n)
