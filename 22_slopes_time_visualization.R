rm(list=ls())
library(tidyverse)
library(directlabels)
library(moderndive)
library(Kendall)
library(trend)
library(gridExtra)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

slope_df = read_csv("03_Auxiliary/Fall 19/slopes4.csv")

level_order = c("Legacy", "Digital-born",
                "Regional", "National", "International",
                "Vernacular", "English")

slope_df %>%
  mutate(MediaType = factor(MediaType, levels = rev(level_order))) -> slope_df


ggplot(slope_df, aes(MediaType, Slope)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = LowerCI, 
                    ymax = UpperCI),
                width = 0.5, size = 0.5) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red")+
  coord_flip()+
  facet_grid(cols = vars(Type)) +
  theme_bw()
