rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(tidyverse)
library(gridExtra)

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

# community wise distribution
KM_ATV_master_df
KM_master_df

load("04_RData/WT2.Rdata")

comm_ATV_tbl = NULL
for(i in 1:length(WT2)) {
  KM_ATV_master_df %>%
    filter(Media %in% WT2[[i]]) %>%
    group_by(Media) %>%
    summarize(MonthlyMean = mean(ATV)) %>%
    ungroup() %>%
    mutate(comm = i) -> curr_comm_ATV
  
  comm_ATV_tbl %>% 
    rbind(curr_comm_ATV) -> comm_ATV_tbl
}

comm_ATV_tbl %>%
  filter(!comm %in% c(10, 11, 12, 13, 14, 15)) -> comm_ATV_tbl2

regional_comms = c(1:4, 6, 9)
international_comms = c(7,8)

comm_ATV_tbl2 %>%
  mutate(comm = replace(comm, comm %in% regional_comms, 20)) %>%
  mutate(comm = replace(comm, comm %in% international_comms, 30)) -> comm_ATV_tbl3

ggplot(comm_ATV_tbl3, aes(x=as.character(comm), y = MonthlyMean)) +
  geom_boxplot()

media_breakdown = read_csv("03_Auxiliary/media_breakdown.csv")
common_nodes %>%
  inner_join(media_breakdown) -> common_nodes_breakdown

# merge with media breakdown
KM_ATV_master_df %>%
  group_by(Media) %>%
  summarize(MeanATV = mean(ATV)) %>%
  ungroup() %>%
  inner_join(common_nodes_breakdown) -> common_nodes_breakdown_ATV

# ATV English v Vernacular
common_nodes_breakdown_ATV %>%
  filter(Indian == "Y") %>%
  select(Media, English, MeanATV) -> EnglishATV

ggplot(EnglishATV, aes(x=English, y=MeanATV)) +
  geom_boxplot()

#ATV regional vs National
common_nodes_breakdown_ATV %>%
  filter(Indian == "Y") %>%
  select(Media, Regional, MeanATV) -> RegionalATV

ggplot(RegionalATV, aes(x=Regional, y=MeanATV)) +
  geom_boxplot()

#ATV Indian vs International
common_nodes_breakdown_ATV %>%
  select(Media, Indian, MeanATV) -> IndianATV

ggplot(IndianATV, aes(x=Indian, y=MeanATV)) +
  geom_boxplot()

#monthly ATV trends
KM_ATV_master_df %>%
  group_by(Month) %>%
  summarize(MeanATV = mean(ATV), MedianATV = median(ATV)) -> monthlyATV

ordered_months = read_csv("03_Auxiliary/ordered_months.csv")

ordered_months %>%
  rename(Month = month) -> ordered_months

monthlyATV %>% 
  inner_join(ordered_months) %>%
  arrange(n) -> monthlyATV

ggplot(monthlyATV, aes(x=n, y=MeanATV)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(monthlyATV, aes(x=n, y=MedianATV)) +
  geom_point() +
  geom_smooth(method = "lm")

# only common nodes
KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) -> KM_ATV_master_df_common

KM_ATV_master_df_common %>%
  group_by(Month) %>%
  summarize(MeanATV = mean(ATV), MedianATV = median(ATV)) %>%
  ungroup() -> monthlyATV

ordered_months = read_csv("03_Auxiliary/ordered_months.csv")

ordered_months %>%
  rename(Month = month) -> ordered_months

monthlyATV %>% 
  inner_join(ordered_months) %>%
  arrange(n) -> monthlyATV

ggplot(monthlyATV, aes(x=n, y=MeanATV)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(monthlyATV, aes(x=n, y=MedianATV)) +
  geom_point() +
  geom_smooth(method = "lm")

m = lm(MedianATV~n, data = monthlyATV)

KM_ATV_master_df_common %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) -> KM_ATV_master_df_n

ggplot(KM_ATV_master_df_n, aes(x=n, y=ATV, group = n)) +
  geom_boxplot() +
  geom_abline(intercept = coef(m)[1], slope = coef(m)[2], color ="red") +
  coord_cartesian(xlim = NULL, ylim = c(0,20)) +
  theme_bw()

# scatterplot between percent reach and ATV
KM_ATV_master_df

KM_master_df = inner_join(KM_ATV_master_df, KM_master_df)

KM_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  group_by(Media) %>%
  summarize(MeanPC = mean(PercentReach), MeanATV = mean(ATV)) %>%
  ungroup() %>%
  inner_join(common_nodes_breakdown) -> media_km

p1 <- ggplot(media_km) +
  geom_point(aes(x=MeanPC, y=MeanATV, color=Regional)) +
  theme_bw()

p2 <- ggplot(media_km) +
  geom_point(aes(x=MeanPC, y=MeanATV, color=Digital)) +
  theme_bw()

p3 <- ggplot(media_km) +
  geom_point(aes(x=MeanPC, y=MeanATV, color=Indian)) +
  theme_bw()

p4 <- ggplot(media_km) +
  geom_point(aes(x=MeanPC, y=MeanATV, color=English)) +
  theme_bw()


grid.arrange(p1, p2, p3, p4, nrow = 2)

# trends in Regional v National

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  filter(Indian == "Y") %>%
  select(n, Month, Media, Regional, ATV) %>%
  group_by(n, Month, Regional) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) -> regional_trends_tbl

ggplot(regional_trends_tbl, aes(x=n, y=medianATV)) +
  geom_point(aes(color = Regional))

ggplot(regional_trends_tbl, aes(x=n, y=meanATV)) +
  geom_point(aes(color = Regional))

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  filter(Indian == "Y") %>%
  select(n, Month, Media, Regional, ATV) -> boxplot_regional_trends_tbl

ggplot(boxplot_regional_trends_tbl) +
    geom_boxplot(aes(x=n, y=ATV, group=interaction(n, Regional), fill = Regional)) +
    coord_cartesian(xlim = NULL, ylim = c(0,20))
    


# trends in Indian v International

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  select(n, Month, Media, Indian, ATV) %>%
  group_by(n, Month, Indian) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) -> indian_trends_tbl

ggplot(indian_trends_tbl, aes(x=n, y=medianATV)) +
  geom_point(aes(color = Indian))

ggplot(indian_trends_tbl, aes(x=n, y=meanATV)) +
  geom_point(aes(color = Indian))

# trends in English v Vernacular

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  filter(Indian == "Y") %>%
  select(n, Month, Media, English, ATV) %>%
  group_by(n, Month, English) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) -> english_trends_tbl

ggplot(english_trends_tbl, aes(x=n, y=medianATV)) +
  geom_point(aes(color = English))

ggplot(english_trends_tbl, aes(x=n, y=meanATV)) +
  geom_point(aes(color = English))

# trends in Indian Digital v Legacy

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  filter(Indian == "Y") %>%
  select(n, Month, Media, Digital, ATV) %>%
  group_by(n, Month, Digital) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) -> i_digital_trends_tbl

ggplot(i_digital_trends_tbl, aes(x=n, y=medianATV)) +
  geom_point(aes(color = Digital))

ggplot(i_digital_trends_tbl, aes(x=n, y=meanATV)) +
  geom_point(aes(color = Digital))

# trends in overall Digital v Legacy

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  select(n, Month, Media, Digital, ATV) %>%
  group_by(n, Month, Digital) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) -> i_digital_trends_tbl

ggplot(i_digital_trends_tbl, aes(x=n, y=medianATV)) +
  geom_point(aes(color = Digital))

ggplot(i_digital_trends_tbl, aes(x=n, y=meanATV)) +
  geom_point(aes(color = Digital))




