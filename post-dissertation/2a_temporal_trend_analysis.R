library(readr)
library(dplyr)
library(igraph)
library(ggplot2)
library(lfe)
library(lmtest)
library(cowplot)
library(stargazer)
library(sandwich)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

KM_master_tbl <- read_csv("03_Auxiliary/Fall 19/km_master.csv")
# all_media_breakdown = read_csv("03_Auxiliary/common_media_breakdown.csv")

ordered_months <- read_csv("03_Auxiliary/Fall 19/ordered_months.csv")
ordered_months %>%
  rename(Month = month) -> ordered_months

media_breakdown <- read_csv("03_Auxiliary/Fall 19/common_nodes_breakdown.csv")

media_master_breakdown <- KM_master_tbl %>%
  merge(media_breakdown) %>%
  select(-X1) %>%
  merge(ordered_months, by = "Month") %>%
  arrange(n, Media)

###########################################################################################
######## digital mean

digital_mean_trends <- media_master_breakdown %>%
  group_by(n, Digital) %>%
  summarise(MeanPC = mean(PercentReach)) %>%
  ungroup() %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy"))

# digital_main_lm <- lm(MeanPC ~ n + as.factor(Digital), digital_trends)

digitalborn_mean_lm <- lm(MeanPC ~ n, digital_mean_trends[digital_mean_trends$Digital == "Digital-born",])

legacy_mean_lm <- lm(MeanPC ~ n, digital_mean_trends[digital_mean_trends$Digital == "Legacy",])

digital_mean_plot <- ggplot(digital_mean_trends, aes(x=n, y=MeanPC, color = Digital)) +
  geom_point(color = "black") +
  scale_color_manual(values = c("Legacy" = "black",
                                "Digital-born" = "red")) +
  facet_grid(~Digital) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = "Mean Percent Reach (%)") +
  theme(legend.position = "none")

######### digital_median

digital_median_trends <- media_master_breakdown %>%
  group_by(n, Digital) %>%
  summarise(MedianPC = median(PercentReach)) %>%
  ungroup() %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy"))

# digital_main_lm <- lm(MedianPC ~ n + as.factor(Digital), digital_trends)

digitalborn_median_lm <- lm(MedianPC ~ n, digital_median_trends[digital_median_trends$Digital == "Digital-born",])

legacy_median_lm <- lm(MedianPC ~ n, digital_median_trends[digital_median_trends$Digital == "Legacy",])

digital_median_plot <- ggplot(digital_median_trends, aes(x=n, y=MedianPC, color = Digital)) +
  geom_point(color = "black") +
  scale_color_manual(values = c("Legacy" = "black",
                                "Digital-born" = "black")) +
  facet_grid(~Digital) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = "Median Percent Reach (%)") +
  theme(legend.position = "none")



# robust / clustered

robust_digital_trends <- media_master_breakdown

felm(PercentReach ~ n + Digital | 0 | 0 | Media, robust_digital_trends) %>%
  summary()

felm(PercentReach ~ n | 0 | 0 | Media, robust_digital_trends[robust_digital_trends$Digital == "Y",]) %>%
  summary()

felm(PercentReach ~ n | 0 | 0 | Media, robust_regional_trends[robust_digital_trends$Digital == "N",]) %>%
  summary()

# english

english_trends <- media_master_breakdown %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  group_by(n, English) %>%
  summarise(MeanPC = mean(PercentReach)) %>%
  ungroup() %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

lm(MeanPC ~ n + as.factor(English), english_trends) %>%
  summary()

lm(MeanPC ~ n, english_trends[english_trends$English == "English",]) %>%
  summary()

lm(MeanPC ~ n, english_trends[english_trends$English == "Vernacular",]) %>%
  summary()

english_plot <- ggplot(english_trends, aes(x=n, y=MeanPC, color = English)) +
  geom_point(color = "black") +
  scale_color_manual(values = c("English" = "skyblue", "Vernacular" = "red")) +
  facet_grid(~English) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = "Mean Percent Reach (%)") +
  theme(legend.position = "none")

# robust / clustered

robust_english_trends <- media_master_breakdown %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

felm(PercentReach ~ n + English | 0 | 0 | Media, robust_english_trends) %>%
  summary()

felm(PercentReach ~ n | 0 | 0 | Media, robust_english_trends[robust_english_trends$English == "English",]) %>%
  summary()

felm(PercentReach ~ n | 0 | 0 | Media, robust_english_trends[robust_english_trends$English == "Vernacular",]) %>%
  summary()

# english indian only

english_indian_trends <- media_master_breakdown %>%
  filter(Indian == "Y") %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  group_by(n, English) %>%
  summarise(MeanPC = mean(PercentReach)) %>%
  ungroup() %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

lm(MeanPC ~ n + as.factor(English), english_indian_trends) %>%
  summary()

lm(MeanPC ~ n, english_indian_trends[english_indian_trends$English == "English",]) %>%
  summary()

lm(MeanPC ~ n, english_indian_trends[english_indian_trends$English == "Vernacular",]) %>%
  summary()

# robust / clustered

robust_english_indian_trends <- media_master_breakdown %>%
  filter(Indian == "Y") %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

felm(PercentReach ~ n + English | 0 | 0 | Media, robust_english_indian_trends) %>%
  summary()

felm(PercentReach ~ n | 0 | 0 | Media, robust_english_indian_trends[robust_english_indian_trends$English == "English",]) %>%
  summary()

felm(PercentReach ~ n | 0 | 0 | Media, robust_english_indian_trends[robust_english_indian_trends$English == "Vernacular",]) %>%
  summary()


# english foreign only

english_foreign_trends <- media_master_breakdown %>%
  filter(Indian == "N") %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  group_by(n, English) %>%
  summarise(MeanPC = mean(PercentReach)) %>%
  ungroup() %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

lm(MeanPC ~ n, english_foreign_trends[english_foreign_trends$English == "Vernacular",]) %>%
  summary()

lm(MeanPC ~ n, english_foreign_trends[english_foreign_trends$English == "English",]) %>%
  summary()

# robust / clustered

robust_english_foreign_trends <- media_master_breakdown %>%
  filter(Indian == "N") %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

felm(PercentReach ~ n + English | 0 | 0 | Media, robust_english_foreign_trends) %>%
  summary()

felm(PercentReach ~ n | 0 | 0 | Media, robust_english_foreign_trends[robust_english_foreign_trends$English == "English",]) %>%
  summary()

felm(PercentReach ~ n | 0 | 0 | Media, robust_english_foreign_trends[robust_english_foreign_trends$English == "Vernacular",]) %>%
  summary()

###############################################################
# audience mobility

load("04_RData/Fall 19/03_filtered_networks.RData")


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

vernacular_media <- c("AP_TG", "AS", "GJ", "HD_RJ_PJ", "JK", "KA_MH", "KR", "OD", "TN", "WB")

# for all vernacular media

v <- vernacular_media

v_m <- media_master_breakdown %>%
  filter(State %in% v) %>%
  pull(Media) %>%
  unique()

all_months_edgelist %>%
  filter(V1 %in% v_m | V2 %in% v_m) %>%         # keep only edges starting from or ending in a specific community
  filter(!(V1 %in% v_m & V2 %in% v_m)) %>%      # remove edges within the community
  
  mutate(VernacularMedia = ifelse(V1 %in% v_m, V1, V2)) %>%
  mutate(OtherMedia = ifelse(V1 %in% v_m, V2, V1)) %>%
  
  select(VernacularMedia, OtherMedia, n, Month, shared_audience) %>%
  inner_join(media_breakdown, by = c("OtherMedia" = "Media")) %>%
  
  inner_join(KM_master_tbl, by = c("Month" = "Month", "VernacularMedia" = "Media")) %>% # join to get UV of Community Media
  select(-PercentReach) %>%                               # drop percent reach
  mutate(PercentOveralap = 100*shared_audience/UV) %>%
  mutate(Digital = replace(Digital, Digital == "N", "Legacy")) %>%
  mutate(Digital = replace(Digital, Digital == "Y", "Digital-born")) %>%
  
  filter(Indian == "Y" & English == "Y") %>% # for audience mobility to Indian outlets
  
  rename(Type = Digital) %>%
  group_by(n, Month, Type) %>%
  summarize(MeanPO = mean(PercentOveralap)) -> all_vernacular_to_national_tbl

v2n_plot <- ggplot(all_vernacular_to_national_tbl, aes(x=n, y=MeanPO, color = Type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = expression(atop("Mean Percent Overlap of Vernacular Media", paste("with National Media (%)"))), col = "Type of National Media") +
  theme(legend.position="bottom")


lm(MeanPO ~ n + Type, all_vernacular_to_national_tbl) %>% summary()

# robust clustered

mobility_national <- all_months_edgelist %>%
  filter(V1 %in% v_m | V2 %in% v_m) %>%         # keep only edges starting from or ending in a specific community
  filter(!(V1 %in% v_m & V2 %in% v_m)) %>%      # remove edges within the community
  
  mutate(VernacularMedia = ifelse(V1 %in% v_m, V1, V2)) %>%
  mutate(OtherMedia = ifelse(V1 %in% v_m, V2, V1)) %>%
  
  select(VernacularMedia, OtherMedia, n, Month, shared_audience) %>%
  inner_join(media_breakdown, by = c("OtherMedia" = "Media")) %>%
  
  inner_join(KM_master_tbl, by = c("Month" = "Month", "VernacularMedia" = "Media")) %>% # join to get UV of Community Media
  select(-PercentReach) %>%                               # drop percent reach
  mutate(PercentOveralap = 100*shared_audience/UV) %>%
  mutate(Digital = replace(Digital, Digital == "N", "Legacy")) %>%
  mutate(Digital = replace(Digital, Digital == "Y", "Digital-born")) %>%
  
  filter(Indian == "Y" & English == "Y") %>% # for audience mobility to Indian outlets
  
  rename(Type = Digital)

felm(PercentOveralap ~ n + Type | 0 | 0 | VernacularMedia, mobility_national) %>% summary()
felm(PercentOveralap ~ n + Type | 0 | 0 | VernacularMedia + OtherMedia, mobility_national) %>% summary()

# all vernacular media to international media

v <- vernacular_media

v_m <- media_master_breakdown %>%
  filter(State %in% v) %>%
  pull(Media) %>%
  unique()

all_months_edgelist %>%
  filter(V1 %in% v_m | V2 %in% v_m) %>%         # keep only edges starting from or ending in a specific community
  filter(!(V1 %in% v_m & V2 %in% v_m)) %>%      # remove edges within the community
  
  mutate(VernacularMedia = ifelse(V1 %in% v_m, V1, V2)) %>%
  mutate(OtherMedia = ifelse(V1 %in% v_m, V2, V1)) %>%
  
  select(VernacularMedia, OtherMedia, n, Month, shared_audience) %>%
  inner_join(media_breakdown, by = c("OtherMedia" = "Media")) %>%
  
  inner_join(KM_master_tbl, by = c("Month" = "Month", "VernacularMedia" = "Media")) %>% # join to get UV of Community Media
  select(-PercentReach) %>%                               # drop percent reach
  mutate(PercentOveralap = 100*shared_audience/UV) %>%
  mutate(Digital = replace(Digital, Digital == "N", "Legacy")) %>%
  mutate(Digital = replace(Digital, Digital == "Y", "Digital-born")) %>%
  
  filter(Indian == "N" & English == "Y") %>% # for audience mobility to Indian outlets
  
  rename(Type = Digital) %>%
  group_by(n, Month, Type) %>%
  summarize(MeanPO = mean(PercentOveralap)) -> all_vernacular_to_international_tbl

v2i_plot <- ggplot(all_vernacular_to_international_tbl, aes(x=n, y=MeanPO, color = Type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = expression(atop("Mean Percent Overlap of Vernacular Media", paste("with International Media (%)"))), col = "Type of International Media") +
  theme(legend.position="bottom")

lm(MeanPO ~ n + as.factor(Type), all_vernacular_to_international_tbl) %>% summary()

# robust / clustered

mobility_international <- all_months_edgelist %>%
  filter(V1 %in% v_m | V2 %in% v_m) %>%         # keep only edges starting from or ending in a specific community
  filter(!(V1 %in% v_m & V2 %in% v_m)) %>%      # remove edges within the community
  
  mutate(VernacularMedia = ifelse(V1 %in% v_m, V1, V2)) %>%
  mutate(OtherMedia = ifelse(V1 %in% v_m, V2, V1)) %>%
  
  select(VernacularMedia, OtherMedia, n, Month, shared_audience) %>%
  inner_join(media_breakdown, by = c("OtherMedia" = "Media")) %>%
  
  inner_join(KM_master_tbl, by = c("Month" = "Month", "VernacularMedia" = "Media")) %>% # join to get UV of Community Media
  select(-PercentReach) %>%                               # drop percent reach
  mutate(PercentOveralap = 100*shared_audience/UV) %>%
  mutate(Digital = replace(Digital, Digital == "N", "Legacy")) %>%
  mutate(Digital = replace(Digital, Digital == "Y", "Digital-born")) %>%
  
  filter(Indian == "N" & English == "Y") %>% # for audience mobility to Indian outlets
  
  rename(Type = Digital)


felm(PercentOveralap ~ n + Type | 0 | 0 | VernacularMedia, mobility_international) %>% summary()

felm(PercentOveralap ~ n + Type | 0 | 0 | VernacularMedia + OtherMedia, mobility_international) %>% summary()


p2 <- plot_grid(region_plot, digital_plot, english_plot, ncol = 1, labels = c("A", "B", "C"))
p3 <- plot_grid(v2n_plot, v2i_plot, ncol = 1, align = "v", labels = c("A", "B"))
