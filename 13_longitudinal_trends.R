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

# network metrics longitudinal trends
library(igraph)

load("04_RData/01_networks.RData")
load("04_RData/02_induced_networks.RData")
load("04_RData/03_filtered_networks.RData")
months = read_csv("03_Auxiliary/months.csv")

graphs_to_use = filtered_graphs_list # or red_graphs_list or filtered_graphs_list

centr_df = NULL
for(i in 1:length(graphs_to_use)) {
  curr_centr = centr_degree(graphs_to_use[[i]])$centralization
  month = as.character(months$months[i])
  month_centr = data.frame(curr_centr, month)
  centr_df = rbind(centr_df, month_centr)
}

centr_df = data.frame(centr_df)
rownames(centr_df) = NULL
names(centr_df) = c("centr", "month")

centr_df$month = as.character(centr_df$month)

ordered_months = read_csv("03_Auxiliary/ordered_months.csv")

centr_df %>%
  inner_join(ordered_months) %>%
  arrange(n) -> centr_df

ggplot(centr_df, aes(x=n, y=centr)) +
  geom_point()+
  geom_smooth(method = "lm")

summary(lm(centr~n, data = centr_df))

####################################################################

all_media_breakdown = read_csv("03_Auxiliary/media_breakdown.csv")
common_media = read_csv("03_Auxiliary/common_nodes.csv")
months = read_csv("03_Auxiliary/months.csv")

all_media_breakdown %>%
  filter(Media %in% common_media$Media) %>%
  select(Media, Digital) -> common_media_digital

graphs_to_use = filtered_graphs_list

temporal_tbl = NULL
for(i in 1:length(graphs_to_use)) {
  g = graphs_to_use[[i]]
  monthly_deg_centralities = data.frame(cbind(V(g)$name), centr_degree(g)$res)
  names(monthly_deg_centralities) = c("Media", "DC")
  monthly_deg_centralities$Media = as.character(monthly_deg_centralities$Media)
  
  
  monthly_deg_centralities %>%
    inner_join(common_media_digital) %>%
    group_by(Digital) %>%
    summarize(MeanDC=mean(DC)) %>%
    mutate(Digital = replace(Digital, Digital == "N", "Legacy")) %>%
    mutate(Digital = replace(Digital, Digital == "Y", "Digital-Born")) %>%
    rename(Type = Digital) %>%
    mutate(Month = months$months[i]) %>%
    rbind(temporal_tbl) -> temporal_tbl
}

temporal_tbl %>%
  inner_join(ordered_months, by = c("Month" = "month")) %>%
  arrange(n) -> temporal_tbl

ggplot(temporal_tbl, aes(x=n, y=log(MeanDC), color = Type)) +
  geom_smooth(method = "lm")
