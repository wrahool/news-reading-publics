rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(tidyverse)
library(gridExtra)
library(igraph)

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

# pearson's

#pointless
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$TotalTime)

#pointless
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalTime)

#this is useful
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV)

#this is useful
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV)

# spearman's

#pointless
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$TotalTime,
         method = "spearman")

#pointless
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalTime,
         method = "spearman")

#useful
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV,
         method = "spearman")

#makes sense
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV,
         method = "spearman")

# spearman's

#pointless
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$TotalTime,
         method = "kendall")

#pointless
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalTime,
         method = "kendall")

#useful
cor.test(total_KM_tbl$TotalUV,
         total_KM_tbl$ATV,
         method = "kendall")

#useful
cor.test(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$TotalUV,
         total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]$ATV,
         method = "kendall")


#distribution of ATV
p1 = ggplot(total_KM_tbl) +
  geom_density(aes(x=ATV))

p2 = ggplot(total_KM_tbl) +
  geom_density(aes(x=Overall_PC))

grid.arrange(p1, p2)

#common nodes only
p1 = ggplot(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]) +
  geom_density(aes(x=ATV)) +
  theme_bw()

p2 = ggplot(total_KM_tbl[total_KM_tbl$Media %in% common_nodes$Media,]) +
  geom_density(aes(x=Overall_PC)) +
  theme_bw()

grid.arrange(p1, p2)

# community wise distribution
KM_ATV_master_df
KM_master_df

load("04_RData/Fall 19/WT2.Rdata")

comm_ATV_tbl = NULL
for(i in 1:max(WT2$membership)) {
  KM_ATV_master_df %>%
    filter(Media %in% WT2$names[WT2$membership == i]) %>%
    group_by(Media) %>%
    summarize(MonthlyMean = mean(ATV)) %>%
    ungroup() %>%
    mutate(comm = i) -> curr_comm_ATV
  
  comm_ATV_tbl %>% 
    rbind(curr_comm_ATV) -> comm_ATV_tbl
}

comm_ATV_tbl %>%
  filter(!comm %in% c(11, 12, 13, 14)) -> comm_ATV_tbl2

ggplot(data = comm_ATV_tbl2) +
  geom_density(aes(x=MonthlyMean), alpha = 0.4) +
  facet_wrap(~comm, nrow = 2) +
  theme_bw()

regional_comms = c(1:3, 5, 8:10)
international_comms = c(6,7)

comm_ATV_tbl2 %>%
  mutate(comm = replace(comm, comm %in% regional_comms, 20)) %>%
  mutate(comm = replace(comm, comm %in% international_comms, 30)) -> comm_ATV_tbl3

ggplot(comm_ATV_tbl3, aes(x=as.character(comm), y = MonthlyMean)) +
  geom_boxplot()

ggplot(comm_ATV_tbl3) +
  geom_density(aes(x=MonthlyMean)) +
  facet_grid(rows = vars(comm)) +
  theme_bw()

ggplot(comm_ATV_tbl3, aes(x=MonthlyMean, fill=as.factor(comm), color = as.factor(comm))) + 
  geom_density(alpha=0.4) +
  theme_bw()

#mann whitney u test: national == regional?
wilcox.test(comm_ATV_tbl3[comm_ATV_tbl3$comm == 20,]$MonthlyMean,
            comm_ATV_tbl3[comm_ATV_tbl3$comm == 4,]$MonthlyMean, alternative = "g")
#median of regional > media of national 

#mann whitney u test: national == international?
wilcox.test(comm_ATV_tbl3[comm_ATV_tbl3$comm == 4,]$MonthlyMean,
            comm_ATV_tbl3[comm_ATV_tbl3$comm == 30,]$MonthlyMean, alternative = "g")
#median of national > median on international

#mann whitney u test: regional == international?
wilcox.test(comm_ATV_tbl3[comm_ATV_tbl3$comm == 20,]$MonthlyMean,
            comm_ATV_tbl3[comm_ATV_tbl3$comm == 30,]$MonthlyMean, alternative = "g")
#median of regional > median on international

media_breakdown = read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")
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

ggplot(EnglishATV, aes(x=MeanATV, fill=as.factor(English), color = as.factor(English))) + 
  geom_density(alpha=0.4)

#put Both as English
EnglishATV %>% mutate(English = ifelse(English %in% c("B", "Y"), "Y", "N")) -> temp
ggplot(temp, aes(x=English, y=MeanATV)) +
  geom_boxplot()

ggplot(temp, aes(x=MeanATV, fill=as.factor(English), color = as.factor(English))) + 
  geom_density(alpha=0.4) +
  theme_bw()

#put Both as Regional
EnglishATV %>% mutate(English = ifelse(English %in% c("B", "N"), "N", "Y")) -> temp
ggplot(temp, aes(x=English, y=MeanATV)) +
  geom_boxplot()
ggplot(temp, aes(x=MeanATV, fill=as.factor(English), color = as.factor(English))) + 
  geom_density(alpha=0.4)

#ATV regional vs National
common_nodes_breakdown_ATV %>%
  filter(Indian == "Y") %>%
  select(Media, Regional, MeanATV) -> RegionalATV

ggplot(RegionalATV, aes(x=Regional, y=MeanATV)) +
  geom_boxplot()

ggplot(RegionalATV, aes(x=MeanATV, fill=as.factor(Regional), color = as.factor(Regional))) + 
  geom_density(alpha=0.4) +
  theme_bw()

#ATV Indian vs International
common_nodes_breakdown_ATV %>%
  select(Media, Indian, MeanATV) -> IndianATV

ggplot(IndianATV, aes(x=Indian, y=MeanATV)) +
  geom_boxplot()
ggplot(IndianATV, aes(x=MeanATV, fill=as.factor(Indian), color = as.factor(Indian))) + 
  geom_density(alpha=0.4) +
  theme_bw()

#ATV Indian Legacy vs Digital-born
common_nodes_breakdown_ATV %>%
  filter(Indian == "Y") %>%
  select(Media, Digital, MeanATV) -> iDigitalATV

ggplot(iDigitalATV, aes(x=Digital, y=MeanATV)) +
  geom_boxplot()
ggplot(iDigitalATV, aes(x=MeanATV, fill=as.factor(Digital), color = as.factor(Digital))) + 
  geom_density(alpha=0.4) +
  theme_bw()

#ATV International Legacy vs Digital-born
common_nodes_breakdown_ATV %>%
  filter(Indian == "N") %>%
  select(Media, Digital, MeanATV) -> intDigitalATV

ggplot(intDigitalATV, aes(x=Digital, y=MeanATV)) +
  geom_boxplot()
ggplot(intDigitalATV, aes(x=MeanATV, fill=as.factor(Digital), color = as.factor(Digital))) + 
  geom_density(alpha=0.4) +
  theme_bw()

#ATV legacy vs digital-born
common_nodes_breakdown_ATV %>%
  select(Media, Digital, MeanATV) -> intDigitalATV

ggplot(intDigitalATV, aes(x=Digital, y=MeanATV)) +
  geom_boxplot()
ggplot(intDigitalATV, aes(x=MeanATV, fill=as.factor(Digital), color = as.factor(Digital))) + 
  geom_density(alpha=0.4) +
  theme_bw()

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

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")

ordered_months %>%
  rename(Month = month) -> ordered_months

monthlyATV %>% 
  inner_join(ordered_months) %>%
  arrange(n) -> monthlyATV

ggplot(monthlyATV, aes(x=n, y=MeanATV)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_bw()

ggplot(monthlyATV, aes(x=n, y=MedianATV)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

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

ggplot(regional_trends_tbl, aes(x=n, y=meanATV, color = Regional)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_bw()

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

ggplot(indian_trends_tbl, aes(x=n, y=meanATV, color = Indian)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()

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

ggplot(english_trends_tbl, aes(x=n, y=meanATV, color = English)) +
  geom_point() +
  geom_smooth(method = "lm")

# put both as regional
KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  filter(Indian == "Y") %>%
  select(n, Month, Media, English, ATV) %>%
  group_by(n, Month, English) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) %>%
  mutate(English = ifelse(English %in% c("B", "N"), "N", "Y")) -> english_trends_tbl

ggplot(english_trends_tbl, aes(x=n, y=meanATV, color = English)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()


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

ggplot(i_digital_trends_tbl, aes(x=n, y=meanATV, color=Digital)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()


# trends in international Digital vs Legacy

KM_ATV_master_df %>%
  filter(Media %in% common_nodes$Media) %>%
  inner_join(ordered_months) %>%
  inner_join(common_nodes_breakdown) %>%
  filter(Indian == "N") %>%
  select(n, Month, Media, Digital, ATV) %>%
  group_by(n, Month, Digital) %>%
  summarize(meanATV = mean(ATV), medianATV = median(ATV)) -> i_digital_trends_tbl

ggplot(i_digital_trends_tbl, aes(x=n, y=medianATV)) +
  geom_point(aes(color = Digital))

ggplot(i_digital_trends_tbl, aes(x=n, y=meanATV, color=Digital)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

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




