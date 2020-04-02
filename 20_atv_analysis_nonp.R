rm(list=ls())
library(tidyverse)
library(directlabels)
library(moderndive)
library(Kendall)
library(trend)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

TI_ATV_df = read_csv("03_Auxiliary/Fall 19/total_internet_atv.csv")
KM_ATV_master_df = read_csv("03_Auxiliary/Fall 19/km_atv_master.csv")

KM_master_df = read_csv("03_Auxiliary/Fall 19/km_master.csv")
TI_df = read_csv("03_Auxiliary/Fall 19/total_internet_uv.csv")

common_nodes = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")
ordered_months %>%
  rename(Month = month) -> ordered_months

media_breakdown = read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")
common_nodes %>%
  inner_join(media_breakdown) -> common_nodes_breakdown

#Digital-born vs legacy

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  select(n, Month, Media, Digital, ATV) %>%
  group_by(n, Month, Digital) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) %>%
  ungroup() -> i_digital_trends_tbl

ggplot(i_digital_trends_tbl, aes(x=n, y=meanATV)) +
  geom_point(aes(color = Digital))

for(t in unique(i_digital_trends_tbl$Digital)) {
  message(t)
  i_digital_trends_tbl %>%
    filter(Digital == t) %>%
    select(meanATV) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("-----------------------")
  
  i_digital_trends_tbl %>%
    filter(Digital == t) %>%
    select(meanATV) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}

#Legacy
#0.2418896 0.2971084

#Digital
#0.1636970 0.2097339

#National, Regional, International

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  mutate(IndianRegional = paste(Indian, Regional, sep="_")) %>%
  select(n, Month, Media, IndianRegional, ATV) %>%
  group_by(n, Month, IndianRegional) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) %>%
  ungroup() -> regional_trends_tbl


ggplot(regional_trends_tbl, aes(x=n, y=meanATV, color = IndianRegional)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_bw()

for(t in unique(regional_trends_tbl$IndianRegional)) {
  message(t)
  regional_trends_tbl %>%
    filter(IndianRegional == t) %>%
    select(meanATV) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("-----------------------")
  
  regional_trends_tbl %>%
    filter(IndianRegional == t) %>%
    select(meanATV) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}

#international
#0.04132661 0.05556806

#national
#0.2652894 0.3385771

#regional
# 0.3436925 0.4604262

#English Vernacular
KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  filter(Indian == "Y") %>%
  select(n, Month, Media, English, ATV) %>%
  group_by(n, Month, English) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) -> english_trends_tbl

ggplot(english_trends_tbl, aes(x=n, y=medianATV)) +
  geom_point(aes(color = English))

ggplot(english_trends_tbl, aes(x=n, y=meanATV, color = English)) +
  geom_point() +
  geom_smooth(method = "lm")

# put both as national
KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  filter(Indian == "Y") %>%
  select(n, Month, Media, English, ATV) %>%
  group_by(n, Month, English) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) %>%
  ungroup() %>%
  mutate(English = ifelse(English %in% c("B", "Y"), "Y", "N")) -> english_trends_tbl

ggplot(english_trends_tbl, aes(x=n, y=meanATV, color = English)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

for(t in unique(english_trends_tbl$English)) {
  message(t)
  english_trends_tbl %>%
    filter(English == t) %>%
    select(meanATV) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("-----------------------")
  
  english_trends_tbl %>%
    filter(English == t) %>%
    select(meanATV) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}

#English
#0.09920823 0.22805281

#Vernacular
#0.4969376 0.6340711
