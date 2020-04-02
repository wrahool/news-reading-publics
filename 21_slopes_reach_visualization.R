rm(list=ls())
library(tidyverse)
library(directlabels)
library(moderndive)
library(Kendall)
library(trend)
library(gridExtra)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

slopes_df = read_csv("03_Auxiliary/Fall 19/slopes1.csv")

plot_list <- vector(mode = "list", length = length(unique(slopes_df$Type2)))
for(t in unique(slopes_df$Type2)) {
  
  plot_df = slopes_df[slopes_df$Type2 == t,]
  
  if(t == 1) {
    level_order = c("Legacy", "Digital-born",
                    "Regional", "National", "International",
                    "Vernacular", "English")
  } else if (t==2) {
    level_order = paste("C1-", 1:6, sep = "")
  } else {
    level_order = paste("C2-", 1:10, sep = "")
  }
  
  plot_df %>%
    mutate(Name = factor(Name, levels = rev(level_order))) %>%
    mutate(Type = factor(Type, levels = c("Parametric", "Nonparametric"))) -> plot_df
  
  
  plot_list[[t]] <- ggplot(plot_df, aes(Name, Slope)) +
    geom_point(size = 1) +
    geom_errorbar(aes(ymin = lowerCI, 
                      ymax = upperCI),
                  width = 0.5, size = 0.5) +
    geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red")+
    coord_flip()+
    facet_grid(cols = vars(Type)) +
    theme_bw()
  
}

grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], nrow=3)

#mobility to National
slope_df <- read_csv("03_Auxiliary/Fall 19/slopes2.csv")

ggplot(slope_df, aes(NationalType, Slope)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = LowerCI, 
                    ymax = UpperCI),
                width = 0.5, size = 0.5) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red")+
  coord_flip()+
  facet_grid(cols = vars(Type), rows = vars(Vernacular)) +
  theme_bw()

#mobility to International
slope_df <- read_csv("03_Auxiliary/Fall 19/slopes3.csv")

ggplot(slope_df, aes(InternationalType, Slope)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = LowerCI, 
                    ymax = UpperCI),
                width = 0.5, size = 0.5) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red")+
  coord_flip()+
  facet_grid(cols = vars(Type), rows = vars(Vernacular)) +
  theme_bw()
