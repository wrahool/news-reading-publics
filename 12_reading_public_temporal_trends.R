rm(list=ls())
library(tidyverse)
library(directlabels)
library(moderndive)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

KM_master_tbl = read_csv("03_Auxiliary/km_master.csv")
all_media_breakdown = read_csv("03_Auxiliary/common_media_breakdown.csv")

ordered_months = read_csv("03_Auxiliary/ordered_months.csv")
ordered_months %>%
  rename(Month = month) -> ordered_months

#regular WT
load("04_RData/WT.Rdata")

community_month_tbl = NULL
for(i in 1:max(WT$membership)) {
  outlets = WT$names[WT$membership == i]
  n_outlets = length(outlets)
  
  KM_master_tbl %>%
    filter(Media %in% outlets) -> curr_outlets_tbl
  
  if(n_outlets * 45 != nrow(curr_outlets_tbl)) {
    message("problem!")
  }
  
  curr_outlets_tbl %>%
    group_by(Month) %>%
    summarize(MeanPC = mean(PercentReach)) -> curr_monthly_mean_tbl
  
  curr_monthly_mean_tbl %>%
    inner_join(ordered_months) %>%
    arrange(n) %>%
    rename(!!paste0("C_",i):= MeanPC) -> curr_comm_trends
  
  bind_cols(community_month_tbl, curr_comm_trends) -> community_month_tbl
}

community_month_tbl %>%
  select(n, Month, paste0("C_", c(1:max(WT$membership)))) -> community_month_tbl

community_month_tbl %>%
  gather(Community, MeanPC, -n, -Month) -> trends_tbl

ggplot(trends_tbl,aes(y = MeanPC,x = n,color = Community)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_dl(aes(label = gsub("C_", "", Community)),
          method = list(dl.trans(x = x + 0.2), 
                        dl.combine("last.points"), cex = 0.8)) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             linetype = "dotted") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

# slopes of meanPC vs n for each community
c = 1
c_estimate_tbl = NULL
for(c in 1:max(WT$membership)) {
  trends_tbl %>%
    filter(Community == paste0("C_",c)) %>%
    select(MeanPC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, p_value) %>%
    mutate(Community = paste0("C_",c)) %>%
    select(Community, estimate, p_value) -> c_estimate
  
  bind_rows(c_estimate_tbl, c_estimate) -> c_estimate_tbl
}

c_estimate_tbl %>%
  mutate(Community = gsub("C_", "", Community)) %>%
  mutate(very_sig = p_value == 0) -> c_estimate_tbl

c_estimate_tbl  


#resolution WT
load("04_RData/WT2.Rdata")

community_month_tbl = NULL
for(i in 1:max(WT2$membership)) {
  outlets = WT2$names[WT2$membership == i]
  n_outlets = length(outlets)
  
  KM_master_tbl %>%
    filter(Media %in% outlets) -> curr_outlets_tbl
  
  if(n_outlets * 45 != nrow(curr_outlets_tbl)) {
    message("problem!")
  }
  
  curr_outlets_tbl %>%
    group_by(Month) %>%
    summarize(MeanPC = mean(PercentReach)) -> curr_monthly_mean_tbl
  
  curr_monthly_mean_tbl %>%
    inner_join(ordered_months) %>%
    arrange(n) %>%
    rename(!!paste0("C_",i):= MeanPC) -> curr_comm_trends
  
  bind_cols(community_month_tbl, curr_comm_trends) -> community_month_tbl
}

community_month_tbl %>%
  select(n, Month, paste0("C_", c(1:max(WT2$membership)))) -> community_month_tbl

community_month_tbl %>%
  gather(Community, MeanPC, -n, -Month) -> trends_tbl

ggplot(trends_tbl,aes(y = MeanPC,x = n,color = Community)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_dl(aes(label = gsub("C_", "", Community)),
          method = list(dl.trans(x = x + 0.2), 
                        dl.combine("last.points"), cex = 0.8)) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             linetype = "dotted") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

# without C12-15
trends_tbl %>% 
  filter(!Community %in% c("C_12", "C_13", "C_14", "C_15")) -> trends_tbl_withoutC1215

ggplot(trends_tbl_withoutC1215, aes(y = MeanPC,x = n,color = Community)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_dl(aes(label = gsub("C_", "", Community)),
          method = list(dl.trans(x = x + 0.2), 
                        dl.combine("last.points"), cex = 0.8)) +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             linetype = "dotted") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = "none")

# slopes of meanPC vs n for each community
c = 1
c_estimate_tbl = NULL
for(c in 1:max(WT2$membership)) {
  trends_tbl %>%
  filter(Community == paste0("C_",c)) %>%
  select(MeanPC, n) %>%
  lm() %>%
  get_regression_table() %>%
  filter(term == "n") %>% # get the row corresponding to n, not the intercept
  select(estimate, p_value) %>%
  mutate(Community = paste0("C_",c)) %>%
  select(Community, estimate, p_value) -> c_estimate
  
  bind_rows(c_estimate_tbl, c_estimate) -> c_estimate_tbl
}

c_estimate_tbl %>%
  mutate(Community = gsub("C_", "", Community)) %>%
  mutate(very_sig = p_value == 0) -> c_estimate_tbl

c_estimate_tbl  
  
