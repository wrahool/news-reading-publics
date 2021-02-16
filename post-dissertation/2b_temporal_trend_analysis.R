library(readr)
library(dplyr)
library(igraph)
library(ggplot2)
library(lfe)
library(lmtest)
library(cowplot)
library(modelsummary)

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

# region wise
# median

regional_median_trends <- media_master_breakdown %>%
  group_by(n, paste0(Regional, Indian)) %>%
  summarise(MedianPC = median(PercentReach)) %>%
  ungroup() %>%
  rename(Region = 2) %>%
  mutate(Region = ifelse(Region == "NN", "International", ifelse(Region == "NY", "National", "Regional")))

regional_main_median_lm <- lm(MedianPC ~ n * Region, regional_median_trends)

f <- function(x) format(round(x, 3), big.mark=",")

modelsummary(list("Median % Reach" = regional_main_median_lm),
             fmt = 5,
             stars = TRUE,
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("n" = "month",
                          "n:RegionNational" = "month x National",
                          "n:RegionRegional" = "month x Regional"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             title = "OLS slope of median audience reach"
)

modelsummary(list("Median % Reach" = digital_main_median_lm),
             fmt = 5,
             stars = TRUE,
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("n" = "month",
                          "n:DigitalLegacy" = "month x Legacy"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             title = "OLS slope of median audience reach",
             output = "06_LaTeXTables/digital_trends_main.tex"
)

national_median_lm <- lm(MedianPC ~ n, regional_median_trends[regional_median_trends$Region == "National",])

international_median_lm <- lm(MedianPC ~ n, regional_median_trends[regional_median_trends$Region == "International",])

regional_median_lm <- lm(MedianPC ~ n, regional_median_trends[regional_median_trends$Region == "Regional",])

modelsummary(list("Median % Reach (International)" = international_median_lm, "Median % Reach (National)" = national_median_lm, "Median % Reach (Regional)" = regional_median_lm),
             fmt = 5,
             stars = c('*' = 0.03, '**' = 0.017, '***' = 0.003),
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             notes = "p-values adjusted for multiple (3) comparisons",
             title = "OLS slope of median audience reach of regional, national, and international media over time"
)

modelsummary(list("Median % Reach (International)" = international_median_lm, "Median % Reach (National)" = national_median_lm, "Median % Reach (Regional)" = regional_median_lm),
             fmt = 5,
             stars = c('*' = 0.03, '**' = 0.017, '***' = 0.003),
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             notes = "p-values adjusted for multiple (3) comparisons",
             title = "OLS slope of median audience reach of regional, national, and international media over time",
             output = "06_LaTeXTables/regional_trends.tex"
)



# colors: black for non-significant, blue for significant increase, red for significant decrease
region_median_plot <- ggplot(regional_median_trends, aes(x=n, y=MedianPC, color = Region)) +
  geom_point(color = "black") +
  scale_color_manual(values = c("National" = "black",
                                "International" = "royalblue", 
                                "Regional" = "red")) +
  facet_grid(~Region) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 14)) +
  labs(x = "Month", y = "median Percent Reach (%)") +
  theme(legend.position = "none")

# robust errors, clustered by media type

robust_regional_trends <- media_master_breakdown %>%
  mutate(Region = paste0(Regional, Indian)) %>%
  mutate(Region = ifelse(Region == "NN", "International", ifelse(Region == "NY", "National", "Regional")))

national_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_regional_trends[robust_regional_trends$Region == "National",])

regional_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_regional_trends[robust_regional_trends$Region == "Regional",])
regional_felm_2 <- felm(PercentReach ~ n | State | 0 | Media, robust_regional_trends[robust_regional_trends$Region == "Regional",])

international_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_regional_trends[robust_regional_trends$Region == "International",])

latex_table_regionrobust <- stargazer(international_felm, national_felm, regional_felm,
                                      align = TRUE, type = "latex",
                                      title = "OLS regression (with robust errors) of Median Percent Reach against Time for Different Types of Media")

save(region_median_plot, file = "04_RData/post-dissertation/region_trends_ggplot.Rdata")
