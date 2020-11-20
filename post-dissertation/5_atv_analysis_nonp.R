rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(tidyverse)
library(trend)
library(cowplot)

TI_ATV_df = read_csv("03_Auxiliary/Fall 19/total_internet_atv.csv")
KM_ATV_master_df = read_csv("03_Auxiliary/Fall 19/km_atv_master.csv")

KM_master_df = read_csv("03_Auxiliary/Fall 19/km_master.csv")
TI_df = read_csv("03_Auxiliary/Fall 19/total_internet_uv.csv")

TI_df %>%
  pull(TotalInternetUV) %>%
  sum() -> TI_UV

KM_master_df %>% 
  inner_join(KM_ATV_master_df) %>%
  select(Month, Media, UV, ATV) %>%
  mutate(TimePerMonth = UV * 1000 * ATV) %>%
  group_by(Media) %>%
  summarise(TotalUV = sum(UV), TotalTime = sum(TimePerMonth)) %>%
  ungroup() %>%
  mutate(ATV = TotalTime/(TotalUV*1000), Overall_PC = 100*TotalUV/TI_UV) %>%
  arrange(desc(Overall_PC)) -> total_KM_tbl

common_nodes = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")

media_breakdown = read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")
common_nodes %>%
  inner_join(media_breakdown) -> common_nodes_breakdown

# merge with media breakdown
KM_ATV_master_df %>%
  group_by(Media) %>%
  summarize(MeanATV = mean(ATV)) %>%
  ungroup() %>%
  inner_join(common_nodes_breakdown) -> common_nodes_breakdown_ATV

##################################################################
#monthly ATV trends
KM_ATV_master_df %>%
  group_by(Month) %>%
  summarize(MeanATV = mean(ATV), MedianATV = median(ATV)) -> monthlyATV

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")

ordered_months %>%
  rename(Month = month) -> ordered_months

monthlyATV %>% 
  inner_join(ordered_months) %>%
  arrange(n) -> monthlyATV

# only common nodes
KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) -> KM_ATV_master_df_common

KM_ATV_master_df_common %>%
  group_by(Month) %>%
  summarize(MeanATV = mean(ATV), MedianATV = median(ATV)) %>%
  ungroup() -> monthlyATV

KM_ATV_master_df_common_breakdown <- KM_ATV_master_df_common %>%
  merge(common_nodes_breakdown) %>%
  mutate(Region = paste0(Indian, English)) %>%
  mutate(Region = ifelse(Region == "YN", "Regional",
                         ifelse(Region == "YY", "English", "International"))) %>%
  mutate(English = ifelse(English %in% c("Y", "B"), "English", "Vernacular")) %>%
  rename(Language = English) %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy")) %>%
  rename(Type = Digital) %>%
  merge(ordered_months)

# regional vs national vs international

KM_regional_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Region) %>%
  summarize(MeanATV = mean(ATV))

regional_atv_res <- tapply(KM_regional_ATV_trends$MeanATV, KM_regional_ATV_trends$Region, FUN = function(x) {
  x %>%
    ts(frequency = 12, start = c(2014, 10)) %>%
    sens.slope(conf.level = 0.95)
    
})

nonp_regional_atv_slopes <- sapply(regional_atv_res, FUN = function(x) {
  c(ci_lower = x$conf.int[1], x$estimate, ci_upper = x$conf.int[2])}) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Region")

regional_atv_p <- ggplot(nonp_regional_atv_slopes, aes(x = Region, y = Sen.s.slope)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), size = 1, width = 0.2) +
  labs(x = "Media Type", y = "Sen's slope for Temporal Trend in Average Time per Visitor per Month") +
  lims(y = c(0, 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw()

# English vs Vernavular

KM_language_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Language) %>%
  summarize(MeanATV = mean(ATV))

language_atv_res <- tapply(KM_language_ATV_trends$MeanATV, KM_language_ATV_trends$Language, FUN = function(x) {
  x %>%
    ts(frequency = 12, start = c(2014, 10)) %>%
    sens.slope(conf.level = 0.95)
  
})

nonp_language_atv_slopes <- sapply(language_atv_res, FUN = function(x) {
  c(ci_lower = x$conf.int[1], x$estimate, ci_upper = x$conf.int[2])}) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Language")

language_atv_p <- ggplot(nonp_language_atv_slopes, aes(x = Language, y = Sen.s.slope)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), size = 1, width = 0.2) +
  labs(x = "Media Type", y = "Sen's slope for Temporal Trend in Average Time per Visitor per Month") +
  lims(y = c(0, 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw()

# Digital-born vs Legacy

KM_type_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Type) %>%
  summarize(MeanATV = mean(ATV))

type_atv_res <- tapply(KM_type_ATV_trends$MeanATV, KM_type_ATV_trends$Type, FUN = function(x) {
  x %>%
    ts(frequency = 12, start = c(2014, 10)) %>%
    sens.slope(conf.level = 0.95)
  
})

nonp_type_atv_slopes <- sapply(type_atv_res, FUN = function(x) {
  c(ci_lower = x$conf.int[1], x$estimate, ci_upper = x$conf.int[2])}) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Type")

type_atv_p <- ggplot(nonp_type_atv_slopes, aes(x = Type, y = Sen.s.slope)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), size = 1, width = 0.2) +
  labs(x = "Media Type", y = "Sen's slope for Temporal Trend in Average Time per Visitor per Month") +
  lims(y = c(0, 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw()

sp3 <- plot_grid(regional_atv_p, language_atv_p, type_atv_p, align = "v", labels = c("A", "B", "C"), ncol = 1)
