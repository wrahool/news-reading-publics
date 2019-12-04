rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(tidyverse)

TI_ATV_df = read_csv("03_Auxiliary/total_internet_atv.csv")
KM_ATV_master_df = read_csv("03_Auxiliary/km_atv_master.csv")

KM_master_df = read_csv("03_Auxiliary/km_master.csv")
TI_df = read_csv("03_Auxiliary/total_internet_uv.csv")

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

common_nodes = read_csv("03_Auxiliary/common_nodes.csv")

# pearson's
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$TotalTime)

cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalTime)

cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV)

cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV)

# spearman's
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$TotalTime,
         method = "spearman")

cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalTime,
         method = "spearman")

cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV,
         method = "spearman")

cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV,
         method = "spearman")

# spearman's
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$TotalTime,
         method = "kendall")

cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalTime,
         method = "kendall")

cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV,
         method = "kendall")

cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV,
         method = "kendall")
