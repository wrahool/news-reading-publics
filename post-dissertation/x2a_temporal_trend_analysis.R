library(readr)
library(dplyr)
library(igraph)
library(ggplot2)
library(lfe)
library(lmtest)
library(cowplot)
library(stargazer)

setwd("C:\\Users\\Subhayan Mukerjee\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

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

############## digital mean (all outlets, not just indian)

digital_mean_trends <- media_master_breakdown %>%
  # filter(Indian == "Y") %>%
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

latex_table_digitalmean <- stargazer(legacy_mean_lm, digitalborn_mean_lm,
                                align = TRUE, type = "latex",
                                title = "OLS regression (with robust errors) of Median Percent Reach against Time for Different Types of Media")


######### digital_median

digital_median_trends <- media_master_breakdown %>%
  # filter(Indian == "Y") %>%
  group_by(n, Digital) %>%
  summarise(MedianPC = median(PercentReach)) %>%
  ungroup() %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy"))

digital_main_median_lm <- lm(MedianPC ~ n * Digital, digital_trends)

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

latex_table_digitalmedian <- stargazer(legacy_median_lm, digitalborn_median_lm,
          align = TRUE, type = "latex",
          title = "OLS regression (with robust errors) of Median Percent Reach against Time for Different Types of Media")

# robust errors

robust_digital_trends <- media_master_breakdown # %>%
  # filter(Indian == "Y")

digitalborn_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_digital_trends[robust_digital_trends$Digital == "Y",])

legacy_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_digital_trends[robust_digital_trends$Digital == "N",])

latex_table_digitalrobust <- stargazer(digitalborn_felm, legacy_felm,
          align = TRUE, type = "latex",
          title = "OLS regression (with robust errors) of Median Percent Reach against Time for Different Types of Media")

save(digital_mean_plot, digital_median_plot, file = "04_RData/post-dissertation/digital_trends_ggplot.Rdata")