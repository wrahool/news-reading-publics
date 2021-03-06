rm(list=ls())

library(tidyverse)
library(directlabels)
library(moderndive)
library(ggplot2)

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

slope_estimate_tbl = NULL
for(t in unique(digital_trends_tbl$Type)) {
  digital_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, p_value) %>%
    mutate(Type = t) %>%
    select(Type, estimate, p_value) %>%
    mutate(Slope = ifelse(estimate < 0,
                          ifelse(p_value <= 0.05,
                                "sig_dec", "dec"),
                          ifelse(p_value <= 0.05,
                                 "sig_inc", "inc"))) -> c_estimate
  
  bind_rows(slope_estimate_tbl, c_estimate) -> slope_estimate_tbl
}

for(t in unique(digital_trends_tbl$Type)) {
  message(t)
  print(digital_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC, n) %>%
    lm() %>%
    summary())
}

digital_trends_tbl %>%
  inner_join(slope_estimate_tbl) %>%
  select(-c(estimate,p_value)) -> digital_trends_tbl

#legacy
0.004390 + (c(-1.96, 1.96) * 0.002178)

#digital born
-0.0086493 + c(c(-1.96, 1.96) * 0.0009783)
  
ggplot(digital_trends_tbl, aes(x=n, y=MeanPC)) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             color = "lightgrey") +
  theme_bw() +
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "none") +
  geom_point() +
  geom_smooth(aes(color = Slope), method = "lm", formula = y ~ x) +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~Type, nrow=1, ncol=2) +
  scale_colour_manual(values = c("sig_inc" = "skyblue1",
                                 "inc" = "skyblue1",
                                 "sig_dec" = "red",
                                 "dec" = "salmon"))
  
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

slope_estimate_tbl = NULL
for(t in unique(geographic_trends_tbl$Type)) {
  geographic_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, p_value) %>%
    mutate(Type = t) %>%
    select(Type, estimate, p_value) %>%
    mutate(Slope = ifelse(estimate < 0,
                          ifelse(p_value <= 0.05,
                                 "sig_dec", "dec"),
                          ifelse(p_value <= 0.05,
                                 "sig_inc", "inc"))) -> c_estimate
  
  bind_rows(slope_estimate_tbl, c_estimate) -> slope_estimate_tbl
}

for(t in unique(geographic_trends_tbl$Type)) {
  message(t)
  print(geographic_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC, n) %>%
    lm() %>%
    summary())
}

#CIs
#national
0.001033 + (c(-1.96, 1.96) * 0.003183)

#international
0.0022743 + (c(-1.96, 1.96) * 0.0008352)

#regional
-0.0018214 + (c(-1.96, 1.96) * 0.0002372)


geographic_trends_tbl %>%
  inner_join(slope_estimate_tbl) %>%
  select(-c(estimate,p_value)) -> geographic_trends_tbl

ggplot(geographic_trends_tbl, aes(x=n, y=MeanPC)) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             color = "lightgrey") +
  theme_bw() +
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "none") +
  geom_point() +
  geom_smooth(aes(color = Slope), method = "lm", formula = y ~ x) +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~Type, nrow=1, ncol=3) +
 
  scale_colour_manual(values = c("sig_inc" = "blue",
                                 "inc" = "skyblue1",
                                 "sig_dec" = "red",
                                 "dec" = "salmon"))


#Panel C
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

slope_estimate_tbl = NULL
for(t in unique(language_trends_tbl$Type)) {
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, lower_ci, upper_ci, p_value) %>%
    mutate(Type = t) %>%
    select(Type, estimate, lower_ci, upper_ci, p_value) %>%
    mutate(Slope = ifelse(estimate < 0,
                          ifelse(p_value <= 0.05,
                                 "sig_dec", "dec"),
                          ifelse(p_value <= 0.05,
                                 "sig_inc", "inc"))) -> c_estimate
  
  bind_rows(slope_estimate_tbl, c_estimate) -> slope_estimate_tbl
}

for(t in unique(language_trends_tbl$Type)) {
  message(t)
  print(language_trends_tbl %>%
          filter(Type == t) %>%
          select(MeanPC, n) %>%
          lm() %>%
          summary())
}

#CIs:
#Vernacular
-0.005679 + (c(-1.96, 1.96) * 0.001092)

#English
0.002804 + (c(-1.96, 1.96) * 0.001395)

language_trends_tbl %>%
  inner_join(slope_estimate_tbl) %>%
  select(-c(estimate,p_value)) -> language_trends_tbl

ggplot(language_trends_tbl, aes(x=n, y=MeanPC)) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             color = "lightgrey") +
  theme_bw() +
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "none") +
  geom_point() +
  geom_smooth(aes(color = Slope), method = "lm", formula = y ~ x) +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~Type, nrow=1, ncol=3) +
  scale_colour_manual(values = c("sig_inc" = "blue",
                                 "inc" = "skyblue1",
                                 "sig_dec" = "red",
                                 "dec" = "salmon"))


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

slope_estimate_tbl = NULL
for(t in unique(language_trends_tbl$Type)) {
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, lower_ci, upper_ci, p_value) %>%
    mutate(Type = t) %>%
    select(Type, estimate, lower_ci, upper_ci, p_value) %>%
    mutate(Slope = ifelse(estimate < 0,
                          ifelse(p_value <= 0.05,
                                 "sig_dec", "dec"),
                          ifelse(p_value <= 0.05,
                                 "sig_inc", "inc"))) -> c_estimate
  
  bind_rows(slope_estimate_tbl, c_estimate) -> slope_estimate_tbl
}

for(t in unique(language_trends_tbl$Type)) {
  message(t)
  print(language_trends_tbl %>%
          filter(Type == t) %>%
          select(MeanPC, n) %>%
          lm() %>%
          summary())
}

#CI
#English  Indian outlets
0.003323 + (c(-1.96, 1.96) * 0.002111)

#

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

slope_estimate_tbl = NULL
for(t in unique(language_trends_tbl$Type)) {
  language_trends_tbl %>%
    filter(Type == t) %>%
    select(MeanPC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, p_value) %>%
    mutate(Type = t) %>%
    select(Type, estimate, p_value) %>%
    mutate(Slope = ifelse(estimate < 0,
                          ifelse(p_value <= 0.05,
                                 "sig_dec", "dec"),
                          ifelse(p_value <= 0.05,
                                 "sig_inc", "inc"))) -> c_estimate
  
  bind_rows(slope_estimate_tbl, c_estimate) -> slope_estimate_tbl
}

for(t in unique(language_trends_tbl$Type)) {
  message(t)
  print(language_trends_tbl %>%
          filter(Type == t) %>%
          select(MeanPC, n) %>%
          lm() %>%
          summary())
}

#CI
#Foreign English
0.0022709 + (c(-1.96, 1.96) * 0.0008463)

##################################################################################
# Need to think more about these results
# Network metrics longitudinal trends
library(igraph)

load("04_RData/Fall 19/01_networks.RData")
load("04_RData/Fall 19/02_induced_networks.RData")
load("04_RData/Fall 19/03_filtered_networks.RData")
months = read_csv("03_Auxiliary/Fall 19/months.csv")

graphs_to_use = filtered_graphs_list # graphs_list or red_graphs_list or filtered_graphs_list

centr_df = NULL
for(i in 1:length(graphs_to_use)) {
  curr_centr = centr_eigen(graphs_to_use[[i]])$centralization
  month = as.character(months$months[i])
  month_centr = data.frame(curr_centr, month)
  centr_df = rbind(centr_df, month_centr)
}

centr_df = data.frame(centr_df)
rownames(centr_df) = NULL
names(centr_df) = c("centr", "month")

centr_df$month = as.character(centr_df$month)

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")

centr_df %>%
  inner_join(ordered_months) %>%
  arrange(n) -> centr_df

ggplot(centr_df, aes(x=n, y=centr)) +
  geom_point()+
  geom_smooth(method = "lm")

summary(lm(centr~n, data = centr_df))

####################################################################

all_media_breakdown = read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")
common_media = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")
Months = read_csv("03_Auxiliary/Fall 19/months.csv")

all_media_breakdown %>%
  filter(Media %in% common_media$Media) %>%
  select(Media, Digital) -> common_media_digital

graphs_to_use = filtered_graphs_list

#
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

slope_estimate_tbl = NULL
for(t in unique(temporal_tbl$Type)) {
  temporal_tbl %>%
    filter(Type == t) %>%
    select(MeanDC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, p_value) %>%
    mutate(Type = t) %>%
    select(Type, estimate, p_value) %>%
    mutate(Slope = ifelse(estimate < 0,
                          ifelse(p_value <= 0.05,
                                 "sig_dec", "dec"),
                          ifelse(p_value <= 0.05,
                                 "sig_inc", "inc"))) -> c_estimate
  
  bind_rows(slope_estimate_tbl, c_estimate) -> slope_estimate_tbl
}

temporal_tbl %>%
  inner_join(slope_estimate_tbl) %>%
  select(-c(estimate,p_value)) -> temporal_tbl

ggplot(temporal_tbl, aes(x=n, y=MeanDC)) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", month)) %>% 
                                 select(n),n), 
             color = "lightgrey") +
  theme_bw() +
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "none") +
  geom_point() +
  geom_smooth(aes(color = Slope), method = "lm", formula = y ~ x) +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~Type, nrow=1, ncol=3) +
  scale_colour_manual(values = c("sig_inc" = "blue",
                                 "inc" = "skyblue1",
                                 "sig_dec" = "red",
                                 "dec" = "salmon"))
