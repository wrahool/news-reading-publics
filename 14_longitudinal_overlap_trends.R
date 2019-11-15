rm(list=ls())

library(tidyverse)
library(directlabels)
library(moderndive)
library(ggplot2)
library(igraph)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

load("04_RData/03_filtered_networks.RData")

KM_master_tbl = read_csv("03_Auxiliary/km_master.csv")

ordered_months = read_csv("03_Auxiliary/ordered_months.csv")
ordered_months %>%
  rename(Month = month) -> ordered_months

all_media_breakdown = read_csv("03_Auxiliary/common_media_breakdown.csv")

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

load("04_RData/WT2.Rdata")

# the regional communities are 1, 3, 4, 6, 7, 9, 10, 11
# international are 5 and 8
# national legacy are  2

regional_communities = c(1,3,4,6,7,9,10,11)

as_tibble(cbind(WT2$names, WT2$membership)) %>%
  rename(Media = V1, Community = V2) -> community_tbl

# public wise trends of digital/legacy media

common_nodes = read_csv("03_Auxiliary/common_nodes.csv")
all_media_breakdown %>%  
  filter(Media %in% common_nodes$Media) %>%
  select(Media, Digital) -> media_digital

community_digital_tbl = NULL

for(i in regional_communities) {

  all_months_edgelist %>%
    filter(V1 %in% WT2[[i]] | V2 %in% WT2[[i]]) %>%         # keep only edges starting from or ending in a specific community
    filter(!(V1 %in% WT2[[i]] & V2 %in% WT2[[i]])) %>%      # remove edges within the community
    
    mutate(CommunityMedia = ifelse(V1 %in% WT2[[i]], V1, V2)) %>%
    mutate(OtherMedia = ifelse(V1 %in% WT2[[i]], V2, V1)) %>%
    
    select(CommunityMedia, OtherMedia, n, Month, shared_audience) %>%
    inner_join(media_digital, by = c("OtherMedia" = "Media")) %>%
    
    mutate(C = i) %>%                                       # add a column with the community id (either V1 or V2 belongs to that community)
    
    inner_join(KM_master_tbl, by = c("Month" = "Month", "CommunityMedia" = "Media")) %>% # join to get UV of Community Media
    select(-PercentReach) %>%                               # drop percent reach
    mutate(PercentOveralap = 100*shared_audience/UV) %>%
    mutate(Digital = replace(Digital, Digital == "N", "Legacy")) %>%
    mutate(Digital = replace(Digital, Digital == "Y", "Digital-born")) %>%
    rename(Type = Digital) %>%
    group_by(C, n, Month, Type) %>%
    summarize(MeanPO = mean(PercentOveralap)) -> community_digital_trends
  
  community_digital_tbl %>%
    rbind(curr_community_tbl) -> community_digital_tbl
}
   

ggplot(community_digital_trends, aes(x=n, y=MeanPO, color = Type)) +
  geom_point() +
  geom_smooth(method = "lm")

# # A tibble: 90 x 5
# # Groups:   C, n, Month [45]
# C     n Month        Type         MeanPO
# <dbl> <dbl> <chr>        <chr>         <dbl>
#   1     1     1 October2014  Digital-born   2.46
#   2     1     1 October2014  Legacy         3.09
#   3     1     2 November2014 Digital-born   1.60
#   4     1     2 November2014 Legacy         2.47
#   5     1     3 December2014 Digital-born   1.81
#   6     1     3 December2014 Legacy         3.07
#   7     1     4 January2015  Digital-born   1.38
#   8     1     4 January2015  Legacy         1.94
#   9     1     5 February2015 Digital-born   2.10
#  10     1     5 February2015 Legacy         2.60
# # ... with 80 more rows


community_digital_tbl %>%
  group_by(n, Month, C, Type) %>%
  summarize(MeanPO = mean(PercentOveralap)) %>%
  select(C, Type, n, Month, MeanPO) %>%
  mutate(C_Type = paste0(C, Type)) -> community_digital_aggregated_tbl

community_digital_aggregated_tbl %>%
  filter(C == 3) -> c_tbl

ggplot(c_tbl, aes(x=n, y=MeanPO, color=C_Type)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             linetype = "dotted") +
  theme_classic()
  
  
curr_community_tbl %>%
  group_by(n, Month, Type) %>%
  summarize(MeanPO = mean(PercentOveralap)) -> temp

ggplot(temp, aes(x=n, y=MeanPO, color = Type)) +
  geom_point() +
  geom_smooth(method = "lm")
