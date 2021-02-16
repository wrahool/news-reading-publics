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

##############################################
# language mean

english_mean_trends <- media_master_breakdown %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  group_by(n, English) %>%
  summarise(MeanPC = mean(PercentReach)) %>%
  ungroup() %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

english_mean_lm <- lm(MeanPC ~ n, english_mean_trends[english_mean_trends$English == "English",])

vernacular_mean_lm <- lm(MeanPC ~ n, english_mean_trends[english_mean_trends$English == "Vernacular",])

english_mean_plot <- ggplot(english_mean_trends, aes(x=n, y=MeanPC, color = English)) +
  geom_point(color = "black") +
  scale_color_manual(values = c("English" = "black", 
                                "Vernacular" = "red")) +
  facet_grid(~English) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = "Mean Percent Reach (%)") +
  theme(legend.position = "none")

latex_table_languagemean <- stargazer(english_mean_lm, vernacular_mean_lm,
                                      align = TRUE, type = "latex",
                                      title = "OLS regression (with standard errors) of Median Percent Reach against Time for Different Types of Media")

# language median

english_median_trends <- media_master_breakdown %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  group_by(n, English) %>%
  summarise(MedianPC = median(PercentReach)) %>%
  ungroup() %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

english_median_lm <- lm(MedianPC ~ n, english_median_trends[english_median_trends$English == "English",])

vernacular_median_lm <- lm(MedianPC ~ n, english_median_trends[english_median_trends$English == "Vernacular",])

english_median_plot <- ggplot(english_median_trends, aes(x=n, y=MedianPC, color = English)) +
  geom_point(color = "black") +
  scale_color_manual(values = c("English" = "black", 
                                "Vernacular" = "red")) +
  facet_grid(~English) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = "Median Percent Reach (%)") +
  theme(legend.position = "none")


latex_table_languagemedian <- stargazer(english_median_lm, vernacular_median_lm,
                                        align = TRUE, type = "latex",
                                        title = "OLS regression (with standard errors) of Median Percent Reach against Time for Different Types of Media")

# Indian english

indian_english_median_trends <- media_master_breakdown %>%
  filter(Indian == "Y") %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  group_by(n, English) %>%
  summarise(MedianPC = median(PercentReach)) %>%
  ungroup() %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

indian_english_median_lm <- lm(MedianPC ~ n, indian_english_median_trends[indian_english_median_trends$English == "English",])

vernacular_median_lm <- lm(MedianPC ~ n, indian_english_median_trends[indian_english_median_trends$English == "Vernacular",])

indian_english_median_plot <- ggplot(indian_english_median_trends, aes(x=n, y=MedianPC, color = English)) +
  geom_point(color = "black") +
  scale_color_manual(values = c("English" = "black", 
                                "Vernacular" = "red")) +
  facet_grid(~English) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = "Median Percent Reach (%)") +
  theme(legend.position = "none")

# robust / clustered

robust_english_trends <- media_master_breakdown %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

english_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_english_trends[robust_english_trends$English == "English",])

vernacular_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_english_trends[robust_english_trends$English == "Vernacular",])

latex_table_robust <- stargazer(english_felm, vernacular_felm,
                                align = TRUE, type = "latex",
                                title = "OLS regression (with standard errors) of Median Percent Reach against Time for Different Types of Media")

save(english_mean_plot, english_median_plot, file = "04_RData/post-dissertation/english_trends_ggplot.Rdata")


