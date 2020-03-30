rm(list=ls())

library(tidyverse)
library(directlabels)
library(moderndive)
library(ggplot2)
library(gridExtra)
library(igraph)
library(broom)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

load("04_RData/Fall 19/03_filtered_networks.RData")

KM_master_tbl = read_csv("03_Auxiliary/Fall 19/km_master.csv")

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")
ordered_months %>%
  rename(Month = month) -> ordered_months

all_media_breakdown = read_csv("03_Auxiliary/Fall 19/common_media_breakdown.csv")

all_months_edgelist = NULL
for(i in 1:length(filtered_graphs_list)) {
  print(i)
  print(ordered_months$Month[i])
  month_network = filtered_graphs_list[[i]]
  
  as_tibble(as_edgelist(month_network)) %>%
    mutate(n = ordered_months$n[i],
           Month = ordered_months$Month[i],
           shared_audience = E(month_network)$shared_audience) -> month_EL
  
  month_EL %>% rbind(all_months_edgelist) -> all_months_edgelist
}

load("04_RData/Fall 19/WT2.Rdata")

# the regional communities are 1, 2, 3, 5, 8, 9, 10
# international are 6 and 7
# national English are  4
# singletons are 11, 12, 13, 14

regional_communities = c(1,2,3,5,8,9,10)

# regional_communities = 1:10

as_tibble(cbind(WT2$names, WT2$membership)) %>%
  rename(Media = V1, Community = V2) -> community_tbl

# public wise trends of national digital/legacy media

common_nodes = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")
all_media_breakdown %>%  
  filter(Media %in% common_nodes$Media) %>%
  filter(Indian == "Y") %>%
  select(Media, Digital) -> media_digital

community_digital_tbl = NULL
community_digital_plots = list()
index = 1
for(i in regional_communities) {
  
  all_months_edgelist %>%
    filter(V1 %in% WT2[[i]] | V2 %in% WT2[[i]]) %>%         # keep only edges starting from or ending in a specific community
    filter(!(V1 %in% WT2[[i]] & V2 %in% WT2[[i]])) %>%      # remove edges within the community
    
    mutate(CommunityMedia = ifelse(V1 %in% WT2[[i]], V1, V2)) %>%
    mutate(OtherMedia = ifelse(V1 %in% WT2[[i]], V2, V1)) %>%
    
    select(CommunityMedia, OtherMedia, n, Month, shared_audience) %>%
    inner_join(media_digital, by = c("OtherMedia" = "Media")) %>%
    
    mutate(C = paste0("Community 2.",i)) %>%                                       # add a column with the community id (either V1 or V2 belongs to that community)
    
    inner_join(KM_master_tbl, by = c("Month" = "Month", "CommunityMedia" = "Media")) %>% # join to get UV of Community Media
    select(-PercentReach) %>%                               # drop percent reach
    mutate(PercentOveralap = 100*shared_audience/UV) %>%
    mutate(Digital = replace(Digital, Digital == "N", "Legacy")) %>%
    mutate(Digital = replace(Digital, Digital == "Y", "Digital-born")) %>%
    rename(Type = Digital) %>%
    group_by(C, n, Month, Type) %>%
    summarize(MeanPO = mean(PercentOveralap)) -> curr_community_tbl
  
  community_digital_tbl %>%
    rbind(curr_community_tbl) -> community_digital_tbl
  
  community_digital_plots[[index]] = ggplot(curr_community_tbl, aes(x=n, y=MeanPO, color = Type)) +
        geom_point() +
        geom_smooth(method = "lm") +
        theme(legend.position="none") +
        ggtitle(paste0("Audience mobility in community ", i))
  
  index = index+1
}

community_digital_tbl %>%
  ungroup() %>%
  mutate(C = as.factor(C)) %>%
  mutate(C = fct_relevel(C, "Community 2.10", after = Inf)) -> community_digital_tbl


ggplot(community_digital_tbl, aes(x=n, y=MeanPO, color = Type)) +
  theme_bw()+
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "bottom") +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n),
             color = "lightgrey")+
  geom_point(size = 0.9) +
  geom_smooth(method = "lm", size = 0.8) +
  scale_x_continuous(breaks = NULL)+
  facet_wrap(~C, nrow = 2)

# get slopes
community_slope_tbl = NULL
for(community in regional_communities) {

  community_digital_tbl %>%
    mutate(Legacy = ifelse(Type == "Legacy", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    select(MeanPO, Legacy, n) %>%
    lm(formula=(MeanPO ~ Legacy+n)) -> m1

  community_digital_tbl %>%
    mutate(Legacy = ifelse(Type == "Legacy", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    select(MeanPO, Legacy, n) %>%
    lm(formula=(MeanPO ~ Legacy*n)) -> m2
  
  m2 %>%
    tidy() %>%
    filter(term == "Legacy:n") %>%
    select(estimate, p.value) %>%
    rename(interaction_slope = estimate) %>%
    mutate(lines_parallel = ifelse(p.value <= 0.05, F, T)) -> interaction_slope
  
  tidy(anova(m1, m2)) %>%
    filter(!is.na(p.value)) %>%
    select(p.value) %>%
    mutate(interaction_model_beter = ifelse(p.value < 0.05, T, F)) -> anova_p_value
  
  community_slope_tbl %>%
    rbind(cbind(community, interaction_slope, anova_p_value)) -> community_slope_tbl
  
  message(community)
  message("Legacy")
  
  #with legacy
  community_digital_tbl %>%
    mutate(Legacy = ifelse(Type == "Legacy", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    filter(Legacy == 1) %>%
    select(MeanPO, n) %>%
    lm(formula=(MeanPO ~ n)) %>%
    summary() %>%
    print()
  
  message(community)
  message("Digital-born")
  
  #with digital
  community_digital_tbl %>%
    mutate(Legacy = ifelse(Type == "Legacy", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    filter(Legacy == 0) %>%
    select(MeanPO, n) %>%
    lm(formula=(MeanPO ~ n)) %>%
    summary() %>%
    print()
}


# do.call(grid.arrange, c(community_digital_plots, list(ncol = 2)))

###################################################################################

# public wise trends of international digital/legacy media

common_nodes = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")
all_media_breakdown %>%  
  filter(Media %in% common_nodes$Media) %>%
  filter(Indian == "N") %>%
  select(Media, Digital) -> media_digital

community_digital_tbl = NULL
community_digital_plots = list()
index = 1
for(i in regional_communities) {
  
  all_months_edgelist %>%
    filter(V1 %in% WT2[[i]] | V2 %in% WT2[[i]]) %>%         # keep only edges starting from or ending in a specific community
    filter(!(V1 %in% WT2[[i]] & V2 %in% WT2[[i]])) %>%      # remove edges within the community
    
    mutate(CommunityMedia = ifelse(V1 %in% WT2[[i]], V1, V2)) %>%
    mutate(OtherMedia = ifelse(V1 %in% WT2[[i]], V2, V1)) %>%
    
    select(CommunityMedia, OtherMedia, n, Month, shared_audience) %>%
    inner_join(media_digital, by = c("OtherMedia" = "Media")) %>%
    
    mutate(C = paste0("Community 2.", i)) %>%                                       # add a column with the community id (either V1 or V2 belongs to that community)
    
    inner_join(KM_master_tbl, by = c("Month" = "Month", "CommunityMedia" = "Media")) %>% # join to get UV of Community Media
    select(-PercentReach) %>%                               # drop percent reach
    mutate(PercentOveralap = 100*shared_audience/UV) %>%
    mutate(Digital = replace(Digital, Digital == "N", "Legacy")) %>%
    mutate(Digital = replace(Digital, Digital == "Y", "Digital-born")) %>%
    rename(Type = Digital) %>%
    group_by(C, n, Month, Type) %>%
    summarize(MeanPO = mean(PercentOveralap)) -> curr_community_tbl
  
  community_digital_tbl %>%
    rbind(curr_community_tbl) -> community_digital_tbl
  
  community_digital_plots[[index]] = ggplot(curr_community_tbl, aes(x=n, y=MeanPO, color = Type)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position="none") +
    ggtitle(paste0("Audience mobility in community ", i))
  
  index = index+1
}

community_digital_tbl %>%
  ungroup() %>%
  mutate(C = as.factor(C)) %>%
  mutate(C = fct_relevel(C, "Community 2.10", after = Inf)) -> community_digital_tbl

ggplot(community_digital_tbl, aes(x=n, y=MeanPO, color = Type)) +
  theme_bw()+
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "bottom")+
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n),
             color = "lightgrey")+
  geom_point(size=0.9) +
  geom_smooth(method = "lm", size=0.8) +
  scale_x_continuous(breaks = NULL)+
  facet_wrap(~C, nrow = 2)


# get slopes
community_slope_tbl = NULL
for(community in regional_communities) {
  
  community_digital_tbl %>%
    mutate(Legacy = ifelse(Type == "Legacy", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    select(MeanPO, Legacy, n) %>%
    lm(formula=(MeanPO ~ Legacy+n)) -> m1
  
  community_digital_tbl %>%
    mutate(Legacy = ifelse(Type == "Legacy", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    select(MeanPO, Legacy, n) %>%
    lm(formula=(MeanPO ~ Legacy*n)) -> m2
  
  m2 %>%
    tidy() %>%
    filter(term == "Legacy:n") %>%
    select(estimate, p.value) %>%
    rename(interaction_slope = estimate) %>%
    mutate(lines_parallel = ifelse(p.value <= 0.05, F, T)) -> interaction_slope
  
  tidy(anova(m1, m2)) %>%
    filter(!is.na(p.value)) %>%
    select(p.value) %>%
    mutate(interaction_model_beter = ifelse(p.value < 0.05, T, F)) -> anova_p_value
  
  community_slope_tbl %>%
    rbind(cbind(community, interaction_slope, anova_p_value)) -> community_slope_tbl
  
  message(community)
  message("Legacy")
  
  #with legacy
  community_digital_tbl %>%
    mutate(Legacy = ifelse(Type == "Legacy", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    filter(Legacy == 1) %>%
    select(MeanPO, n) %>%
    lm(formula=(MeanPO ~ n)) %>%
    summary() %>%
    print()
  
  message(community)
  message("Digital-born")
  
  #with digital
  community_digital_tbl %>%
    mutate(Legacy = ifelse(Type == "Legacy", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    filter(Legacy == 0) %>%
    select(MeanPO, n) %>%
    lm(formula=(MeanPO ~ n)) %>%
    summary() %>%
    print()
  
  
}


###################################################################################

# public wise trends of nationa/international

common_nodes = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")
all_media_breakdown %>%  
  filter(Media %in% common_nodes$Media) %>%
  select(Media, Indian)-> media_geo

community_geo_tbl = NULL
community_geo_plots = list()
index = 1
for(i in regional_communities) {
  
  all_months_edgelist %>%
    filter(V1 %in% WT2[[i]] | V2 %in% WT2[[i]]) %>%         # keep only edges starting from or ending in a specific community
    filter(!(V1 %in% WT2[[i]] & V2 %in% WT2[[i]])) %>%      # remove edges within the community
    
    mutate(CommunityMedia = ifelse(V1 %in% WT2[[i]], V1, V2)) %>%
    mutate(OtherMedia = ifelse(V1 %in% WT2[[i]], V2, V1)) %>%
    
    select(CommunityMedia, OtherMedia, n, Month, shared_audience) %>%
    inner_join(media_geo, by = c("OtherMedia" = "Media")) %>%
    
    mutate(C = paste0("Community 2.", i)) %>%                                       # add a column with the community id (either V1 or V2 belongs to that community)
    
    inner_join(KM_master_tbl, by = c("Month" = "Month", "CommunityMedia" = "Media")) %>% # join to get UV of Community Media
    select(-PercentReach) %>%                               # drop percent reach
    mutate(PercentOveralap = 100*shared_audience/UV) %>%
    mutate(Indian = replace(Indian, Indian == "N", "Foreign")) %>%
    mutate(Indian = replace(Indian, Indian == "Y", "Indian")) %>%
    rename(Type = Indian) %>%
    group_by(C, n, Month, Type) %>%
    summarize(MeanPO = mean(PercentOveralap)) -> curr_community_tbl
  
  community_geo_tbl %>%
    rbind(curr_community_tbl) -> community_geo_tbl
  
  community_geo_plots[[index]] = ggplot(curr_community_tbl, aes(x=n, y=MeanPO, color = Type)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position="none") +
    ggtitle(paste0("Audience mobility in community ", i))
  
  index = index+1
}

community_geo_tbl %>%
  ungroup() %>%
  mutate(C = as.factor(C)) %>%
  mutate(C = fct_relevel(C, "Community 2.10", after = Inf)) -> community_geo_tbl

ggplot(community_geo_tbl, aes(x=n, y=MeanPO, color = Type)) +
  theme_bw()+
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "bottom") +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n),
             color = "lightgrey")+
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = NULL)+
  facet_wrap(~C, nrow = 2)

# get slopes
community_slope_tbl = NULL
for(community in regional_communities) {
  
  community_geo_tbl %>%
    mutate(Foreign = ifelse(Type == "Foreign", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    select(MeanPO, Foreign, n) %>%
    lm(formula=(MeanPO ~ Foreign+n)) -> m1
  
  community_geo_tbl %>%
    mutate(Foreign = ifelse(Type == "Foreign", 1, 0)) %>%
    filter(C==paste0("Community 2.", community)) %>%
    select(MeanPO, Foreign, n) %>%
    lm(formula=(MeanPO ~ Foreign*n)) -> m2
  
  m2 %>%
    tidy() %>%
    filter(term == "Foreign:n") %>%
    select(estimate, p.value) %>%
    rename(interaction_slope = estimate) %>%
    mutate(lines_parallel = ifelse(p.value <= 0.05, F, T)) -> interaction_slope
  
  tidy(anova(m1, m2)) %>%
    filter(!is.na(p.value)) %>%
    select(p.value) %>%
    mutate(interaction_model_beter = ifelse(p.value < 0.05, T, F)) -> anova_p_value
  
  community_slope_tbl %>%
    rbind(cbind(community, interaction_slope, anova_p_value)) -> community_slope_tbl
}

community_slope_tbl

# findings:
# for national news, the mobility is greater from regional to legacy than regional to digital-born
# for international news, the mobility is greater from regional to digital-born than regional to national.
# between national news and international news, the mobility is greater from regional to national news, than from regional to international news

