rm(list=ls())

library(tidyverse)
library(directlabels)
library(moderndive)
library(ggplot2)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

KM_master_tbl = read_csv("03_Auxiliary/km_master.csv")
all_media_breakdown = read_csv("03_Auxiliary/common_media_breakdown.csv")
common_media = read_csv("03_Auxiliary/common_nodes.csv")

# check that there are 177 outlets

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media) %>%
  pull(Media) %>%
  unique() %>%
  length() == 177

# check that the only Fishy outlet is THEFAMOUSPEOPLE.COM

KM_master_tbl %>%
  inner_join(all_media_breakdown) %>%
  select(-X1) %>%
  filter(Media %in% common_media$Media) %>% 
  filter(Fishy == 'Y') %>%
  pull(Media) %>%
  unique() == "THEFAMOUSPEOPLE.COM"

ordered_months = read_csv("03_Auxiliary/ordered_months.csv")
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
  rename(Type=Digital) -> digital_trends_tbl
  
ggplot(digital_trends_tbl, aes(x=n, y=MeanPC, color=Type)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             linetype = "dotted") +
  theme_classic()
  
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
  rename(Type=State) -> geographic_trends_tbl

ggplot(geographic_trends_tbl, aes(x=n, y=MeanPC, color=Type)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             linetype = "dotted") +
  theme_classic()






