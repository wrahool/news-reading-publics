library(readr)
library(dplyr)
library(igraph)
library(ggplot2)
library(lfe)
library(lmtest)
library(cowplot)
library(stargazer)
library(modelsummary)

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

######### digital_median

digital_median_trends <- media_master_breakdown %>%
  # filter(Indian == "Y") %>%
  group_by(n, Digital) %>%
  summarise(MedianPC = median(PercentReach)) %>%
  ungroup() %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy"))

digital_main_median_lm <- lm(MedianPC ~ n * Digital, digital_median_trends)

f <- function(x) format(round(x, 3), big.mark=",")

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

digitalborn_median_lm <- lm(MedianPC ~ n, digital_median_trends[digital_median_trends$Digital == "Digital-born",])

legacy_median_lm <- lm(MedianPC ~ n, digital_median_trends[digital_median_trends$Digital == "Legacy",])

modelsummary(list("Median % Reach (Digital-born)" = digitalborn_median_lm, "Median % Reach (Legacy)" = legacy_median_lm),
             fmt = 5,
             stars = c('*' = 0.05, '**' = 0.025, '***' = 0.005),
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             title = "OLS slope of median audience reach of digital-born and legacy media over time",
             notes = "p-values adjusted for multiple (2) comparisons"
             )

modelsummary(list("Median % Reach (Digital-born)" = digitalborn_median_lm, "Median % Reach (Legacy)" = legacy_median_lm),
             fmt = 5,
             stars = c('*' = 0.05, '**' = 0.025, '***' = 0.005),
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             title = "OLS slope of median audience reach of digital-born and legacy media over time",
             notes = "p-values adjusted for multiple (2) comparisons",
            output = "06_LaTeXTables/digital_trends.tex",
)


digital_median_plot <- ggplot(digital_median_trends, aes(x=n, y=MedianPC, color = Digital)) +
  geom_point(color = "black") +
  scale_color_manual(values = c("Legacy" = "black",
                                "Digital-born" = "black")) +
  facet_grid(~Digital) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 14)) +
  labs(x = "Month", y = "Median Percent Reach (%)") +
  theme(legend.position = "none")

save(digital_median_plot, file = "04_RData/post-dissertation/digital_trends_ggplot.Rdata")


###############################################################################
# robust errors

robust_digital_trends <- media_master_breakdown # %>%
# filter(Indian == "Y")

digitalborn_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_digital_trends[robust_digital_trends$Digital == "Y",])

legacy_felm <- felm(PercentReach ~ n | 0 | 0 | Media, robust_digital_trends[robust_digital_trends$Digital == "N",])

latex_table_digitalrobust <- stargazer(digitalborn_felm, legacy_felm,
                                       align = TRUE, type = "latex",
                                       title = "OLS regression (with robust errors) of Median Percent Reach against Time for Different Types of Media")

save(digital_mean_plot, digital_median_plot, file = "04_RData/post-dissertation/digital_trends_ggplot.Rdata")