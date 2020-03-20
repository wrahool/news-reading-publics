rm(list=ls())

library(tidyverse)
library(directlabels)
library(moderndive)
library(ggplot2)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

KM_master_tbl = read_csv("03_Auxiliary/Fall 19/km_master.csv")
all_media_breakdown = read_csv("03_Auxiliary/Fall 19/common_media_breakdown.csv")
common_media = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")

KM_master_tbl %>%
  filter(Media %in% common_media$Media) %>%
  group_by(Media) %>%
  summarize(MeanPR = mean(PercentReach)) %>%
  arrange(desc(MeanPR)) %>%
  head(21) -> viz_df

ggplot(viz_df) +
  geom_col(aes(x=Media, y=MeanPR))
