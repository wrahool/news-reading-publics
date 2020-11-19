rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(tidyverse)
library(gridExtra)
library(igraph)
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

#this is useful
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV)

#this is useful
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV)

# spearman's

#useful
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV,
         method = "spearman")

#makes sense
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV,
         method = "spearman")

# spearman's

#useful
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV,
         method = "kendall")

#useful
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV,
         method = "kendall")


#common nodes only
atv_density = ggplot(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]) +
  geom_density(aes(x=ATV)) +
  theme_bw()


media_breakdown = read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")
common_nodes %>%
  inner_join(media_breakdown) -> common_nodes_breakdown

# merge with media breakdown
KM_ATV_master_df %>%
  group_by(Media) %>%
  summarize(MeanATV = mean(ATV)) %>%
  ungroup() %>%
  inner_join(common_nodes_breakdown) -> common_nodes_breakdown_ATV


# international vs national vs regional

regional_atv <- common_nodes_breakdown_ATV %>%
  mutate(IndianEnglish = paste0(Indian, English)) %>%
  select(-Indian, -English) %>%
  rename(Region = IndianEnglish) %>%
  mutate(Region = ifelse(Region == "YY", "National",
                         ifelse(Region == "YN", "Vernacular", "International")))

regional_p <- ggplot(regional_atv, aes(x=MeanATV, fill=Region, color = Region)) + 
  geom_density(alpha=0.4) +
  theme_bw()

# english vs vernacular

language_atv <- common_nodes_breakdown_ATV %>%
  mutate(English = ifelse(English %in% c("Y", "B"), "English", "Vernacular")) %>%
  rename(Language = English)

language_p <- ggplot(language_atv, aes(x=MeanATV, fill=Language, color = Language)) +
  geom_density(alpha = 0.4) +
  theme_bw()

# Digital-born vs legacy

digital_atv <- common_nodes_breakdown_ATV %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy")) %>%
  rename(Type = Digital)

digital_p <- ggplot(digital_atv, aes(x=MeanATV, fill = Type, color = Type)) +
  geom_density(alpha = 0.4) +
  theme_bw()

plot_grid(regional_p, language_p, digital_p, align = "v", labels = c("A", "B", "C"), ncol = 1)


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

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")

ordered_months %>%
  rename(Month = month) -> ordered_months

KM_ATV_master_df_common_breakdown <- KM_ATV_master_df_common %>%
  merge(common_nodes_breakdown) %>%
  mutate(Region = paste0(Indian, English)) %>%
  mutate(Region = ifelse(Region == "YN", "Regional",
                         ifelse(Region == "YY", "English", "International"))) %>%
  mutate(English = ifelse(English %in% c("Y", "B"), "English", "Vernacular")) %>%
  rename(Language = English) %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Lgeacy")) %>%
  rename(Type = Digital) %>%
  merge(ordered_months)

# regional vs national vs international

KM_regional_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Region) %>%
  summarize(MeanATV = mean(ATV))

ggplot(KM_regional_ATV_trends, aes(x=n, y=MeanATV)) +
  geom_point(color = "black") +
  facet_grid(~Region) +
  geom_smooth(method = "lm") +
  theme_bw()


# English vs Vernavular

KM_language_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Language) %>%
  summarize(MeanATV = mean(ATV))

ggplot(KM_language_ATV_trends, aes(x=n, y=MeanATV)) +
  geom_point(color = "black") +
  facet_grid(~Language) +
  geom_smooth(method = "lm") +
  theme_bw()


# Digital-born vs Legacy

KM_type_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Type) %>%
  summarize(MeanATV = mean(ATV))

ggplot(KM_type_ATV_trends, aes(x=n, y=MeanATV)) +
  geom_point(color = "black") +
  facet_grid(~Type) +
  geom_smooth(method = "lm") +
  theme_bw()

# Indian digital-born vs Legacy

KM_i_type_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  filter(Region %in% c("Regional", "National")) %>%
  group_by(n, Type) %>%
  summarize(MeanATV = mean(ATV))

ggplot(KM_i_type_ATV_trends, aes(x=n, y=MeanATV)) +
  geom_point(color = "black") +
  facet_grid(~Type) +
  geom_smooth(method = "lm") +
  theme_bw()




