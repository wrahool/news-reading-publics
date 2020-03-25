rm(list=ls())

library(tidyverse)
library(directlabels)
library(moderndive)
library(ggplot2)
library(Kendall)
library(trend)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

KM_master_tbl = read_csv("03_Auxiliary/Fall 19/km_master.csv")
all_media_breakdown = read_csv("03_Auxiliary/Fall 19/common_media_breakdown.csv")
common_media = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")

# check that there are 174 outlets

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media) %>%
  pull(Media) %>%
  unique() %>%
  length() == 174

# check that the only Fishy outlet is THEFAMOUSPEOPLE.COM

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media) %>% 
  filter(Fishy == 'Y') %>%
  pull(Media) %>%
  unique() == "THEFAMOUSPEOPLE.COM"

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")
ordered_months %>%
  rename(Month = month) -> ordered_months

# Legacy vs digital-born Indian media

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media, Indian == "Y") %>% 
  select(Month, Media, PercentReach, Digital) %>%
  inner_join(ordered_months) %>%
  select(n, Month, Media, everything()) %>%
  arrange(n) %>%
  group_by(n, Month, Digital) %>%
  summarize(MeanPC = mean(PercentReach)) %>%
  mutate(Digital=replace(Digital, Digital=="N", "Legacy")) %>%
  mutate(Digital=replace(Digital, Digital=="Y", "Digital-Born")) %>%
  ungroup() %>%
  rename(Type=Digital) -> digital_trends_tbl

digital_trends_tbl %>%
  select(n, Type, MeanPC)

for(t in unique(digital_trends_tbl$Type)) {
  message(t)
  digital_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  print("-------------------------")
  digital_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}

# Indian National vs Indian Regional vs International media

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media) %>% 
  select(Month, Media, PercentReach, State) %>%
  inner_join(ordered_months) %>%
  select(n, Month, Media, everything()) %>%
  arrange(n) %>%
  mutate(State=replace(State, !State %in% c("None", "NT"), "Regional")) %>%
  mutate(State=replace(State, State == "NT", "National")) %>%
  mutate(State=replace(State, State == "None", "International")) %>%
  group_by(n, Month, State) %>%
  summarize(MeanPC = mean(PercentReach)) %>%
  rename(Type=State) %>%
  ungroup() -> geographic_trends_tbl

geographic_trends_tbl %>%
  select(n, Type, MeanPC)

for(t in unique(geographic_trends_tbl$Type)) {
  message(t)
  geographic_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("-------------------------")
  
  geographic_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}

# English vs Vernacular

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media) %>% 
  select(Month, Media, PercentReach, English) %>%
  inner_join(ordered_months) %>%
  select(n, Month, Media, everything()) %>%
  arrange(n) %>%
  mutate(English=replace(English, English %in% c("Y", "B"), "English")) %>%
  mutate(English=replace(English, English == "N", "Vernacular")) %>%
  group_by(n, Month, English) %>%
  summarize(MeanPC = mean(PercentReach)) %>%
  rename(Type=English) %>%
  ungroup() -> language_trends_tbl

language_trends_tbl %>%
  select(n, Type, MeanPC)

for(t in unique(language_trends_tbl$Type)) {
  message(t)
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("---------------------------")
  
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
    
}

#remove foreign outlets from English outlets, and only keep Indian outlets

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media, Indian == "Y") %>% 
  select(Month, Media, PercentReach, English) %>%
  inner_join(ordered_months) %>%
  select(n, Month, Media, everything()) %>%
  arrange(n) %>%
  mutate(English=replace(English, English %in% c("Y", "B"), "English")) %>%
  mutate(English=replace(English, English == "N", "Vernacular")) %>%
  group_by(n, Month, English) %>%
  summarize(MeanPC = mean(PercentReach)) %>%
  rename(Type=English) %>%
  ungroup() -> language_trends_tbl

language_trends_tbl %>%
  select(n, Type, MeanPC)

for(t in unique(language_trends_tbl$Type)) {
  message(t)
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("----------------------------")
  
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}

# foreign English outlets

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media, Indian == "N") %>% 
  select(Month, Media, PercentReach, English) %>%
  inner_join(ordered_months) %>%
  select(n, Month, Media, everything()) %>%
  arrange(n) %>%
  mutate(English=replace(English, English %in% c("Y", "B"), "English")) %>%
  mutate(English=replace(English, English == "N", "Vernacular")) %>%
  group_by(n, Month, English) %>%
  summarize(MeanPC = mean(PercentReach)) %>%
  rename(Type=English) %>%
  ungroup() -> language_trends_tbl

language_trends_tbl %>%
  select(n, Type, MeanPC)

for(t in unique(language_trends_tbl$Type)) {
  message(t)
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("--------------------")
  
  
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}
