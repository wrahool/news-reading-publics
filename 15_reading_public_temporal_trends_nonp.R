rm(list=ls())
library(tidyverse)
library(directlabels)
library(moderndive)
library(Kendall)
library(trend)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

KM_master_tbl = read_csv("03_Auxiliary/Fall 19/km_master.csv")
# all_media_breakdown = read_csv("03_Auxiliary/common_media_breakdown.csv")

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")
ordered_months %>%
  rename(Month = month) -> ordered_months

#regular WT
load("04_RData/Fall 19/WT.Rdata")

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
    rename(!!paste0("Community 1.",i):= MeanPC) -> curr_comm_trends
  
  bind_cols(community_month_tbl, curr_comm_trends) -> community_month_tbl
}

community_month_tbl %>%
  select(n, Month, paste0("Community 1.", c(1:max(WT$membership)))) -> community_month_tbl

community_month_tbl %>%
  gather(Community, MeanPC, -n, -Month) -> trends_tbl


# slopes of meanPC vs n for each community
c = 1
c_estimate_tbl = NULL
for(c in 1:max(WT$membership)) {
  trends_tbl %>%
    filter(Community == paste0("Community 1.",c)) %>%
    select(MeanPC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, p_value) %>%
    mutate(Community = paste0("Community 1.",c)) %>%
    select(Community, estimate, p_value) -> c_estimate
  
  bind_rows(c_estimate_tbl, c_estimate) -> c_estimate_tbl
}

c_estimate_tbl

c_estimate_tbl %>%
  mutate(slope = ifelse(estimate < 0,
                        ifelse(p_value <= 0.05, 
                               "sig_dec", "dec"),
                        ifelse(p_value <= 0.05,
                               "sig_inc", "inc"))) -> community_colors

trends_tbl %>% inner_join(community_colors) ->
  trends_tbl

ggplot(trends_tbl, aes(y=MeanPC, x=n)) +
  theme_bw()+
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "none") +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             color = "lightgrey") +
  geom_point(size=0.01) +
  geom_smooth(method = "lm", aes(color = slope)) +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~Community, nrow=2, ncol=3) +
  scale_colour_manual(values = c("sig_inc" = "blue",
                                 "inc" = "skyblue1",
                                 "sig_dec" = "red",
                                 "dec" = "salmon"))


for(c in unique(trends_tbl$Community)) {
  message(c)
  trends_tbl %>%
    filter(Community == c) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("-----------------------")
  
  trends_tbl %>%
    filter(Community == c) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}



####################################################################################
#resolution WT
load("04_RData/Fall 19/WT2.Rdata")

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
    rename(!!paste0("Community 2.",i):= MeanPC) -> curr_comm_trends
  
  bind_cols(community_month_tbl, curr_comm_trends) -> community_month_tbl
}

community_month_tbl %>%
  select(n, Month, paste0("Community 2.", c(1:max(WT2$membership)))) -> community_month_tbl

community_month_tbl %>%
  gather(Community, MeanPC, -n, -Month) -> trends_tbl

# without C11-14
trends_tbl %>% 
  filter(!Community %in% c("Community 2.11", "Community 2.12", "Community 2.13", "Community 2.14")) -> trends_tbl_without_singles

# slopes of meanPC vs n for each community
c = 1
c_estimate_tbl = NULL
for(c in 1:max(WT2$membership)) {
  trends_tbl %>%
    filter(Community == paste0("Community 2.",c)) %>%
    select(MeanPC, n) %>%
    lm() %>%
    get_regression_table() %>%
    filter(term == "n") %>% # get the row corresponding to n, not the intercept
    select(estimate, p_value) %>%
    mutate(Community = paste0("Community 2.",c)) %>%
    select(Community, estimate, p_value) -> c_estimate
  
  bind_rows(c_estimate_tbl, c_estimate) -> c_estimate_tbl
}

for(c in 1:max(WT2$membership)) {
  message(c)
  trends_tbl %>%
    filter(Community == paste0("Community 2.",c)) %>%
    select(MeanPC, n) %>%
    lm() %>%
    summary() %>% print()
}

# add slope colours
# if significant increase then blue
# if non-significant increase then skyblue
# if significant decrease then red
# if non-significant decrease then salmon.


c_estimate_tbl %>%
  mutate(slope = ifelse(estimate < 0,
                        ifelse(p_value < 0.05, 
                               "sig_dec", "dec"),
                        ifelse(p_value < 0.05,
                               "sig_inc", "inc"))) -> community_colors

trends_tbl_without_singles %>% inner_join(community_colors) ->
  trends_tbl_without_singles

# put level Community 2.10 at the end
trends_tbl_without_singles %>%
  mutate(Community = as.factor(Community)) %>%
  mutate(Community = fct_relevel(Community, "Community 2.10", after = Inf)) -> 
  trends_tbl_without_singles

ggplot(trends_tbl_without_singles, aes(y=MeanPC, x=n)) +
  theme_bw()+
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position = "none") +
  geom_vline(xintercept = pull(ordered_months %>% 
                                 filter(grepl("January", Month)) %>% 
                                 select(n),n), 
             color = "lightgrey") +
  geom_point(size=0.01) +
  geom_smooth(method = "lm", aes(color = slope)) +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~Community, nrow=2, ncol=5) +
  scale_colour_manual(values = c("sig_inc" = "blue",
                                 "inc" = "skyblue1",
                                 "sig_dec" = "red",
                                 "dec"="salmon"))


for(c in unique(trends_tbl_without_singles$Community)) {
  message(c)
  trends_tbl_without_singles %>%
    filter(Community == c) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    MannKendall() %>%
    summary()
  
  print("-----------------------")
  
  trends_tbl_without_singles %>%
    filter(Community == c) %>%
    select(MeanPC) %>%
    ts(frequency = 12, start = c(2014,10)) %>%
    sens.slope() %>%
    print()
}

