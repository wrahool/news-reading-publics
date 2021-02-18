setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(readr)
library(dplyr)
library(gridExtra)
library(igraph)
library(lfe)
library(ggplot2)
library(modelsummary)

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
  mutate(ATV = TotalTime/(TotalUV*1000)) %>%
  select(Media, ATV) -> ATV_tbl

KM_master_df %>%
  group_by(Media) %>%
  summarise(MeanPR = mean(PercentReach)) %>%
  ungroup() %>%
  merge(ATV_tbl) -> scatterplot_tbl

common_nodes = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")

media.breakdown <- read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")

scatterplot_tbl <- scatterplot_tbl %>%
  filter(Media %in% common_nodes$Media) %>%
  merge(media.breakdown) %>%
  mutate(RegionalIndian = paste0(Regional, Indian)) %>%
  mutate(RegionalIndian = ifelse(RegionalIndian == "NN", "International",
                                 ifelse(RegionalIndian == "YY", "Regional", "National"))) %>%
  select(-Regional, -Indian) %>%
  rename(Region = RegionalIndian) %>%
  mutate(English = ifelse(English == "Y", "English",
                          ifelse(English == "N", "Vernacular", "Both"))) %>%
  rename(Language = English) %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy")) %>%
  rename(Type = Digital) %>%
  select(-c(State, News, Social, Aggregator, Fishy, Group, Relevant)) %>%
  as_tibble()

regional_sp <- ggplot(scatterplot_tbl) +
  geom_point(aes(x=log(MeanPR), y=log(ATV), color=Region), size = 2, shape = 16) +
  scale_color_brewer(palette="Set1") +
  labs(x = "log(mean % reach)", y = "log(mean avg. time \n per visitor)") +
  theme_bw() +
  theme(legend.position="right")

digital_sp <- ggplot(scatterplot_tbl) +
  geom_point(aes(x=log(MeanPR), y=log(ATV), color=Type), size = 2, shape = 16) +
  scale_color_manual(values=wes_palette(name="Royal1")) +
  labs(x = "log(mean % reach)", y = "log(mean avg. time \n per visitor )") +
  theme_bw() +
  theme(legend.position="right")

language_sp <- ggplot(scatterplot_tbl) +
  geom_point(aes(x=log(MeanPR), y=log(ATV), color=Language), size = 2, shape = 16) +
  scale_color_manual(values=wes_palette(name="GrandBudapest1")) +
  labs(x = "log(mean % reach)", y = "log(mean avg. time \n per visitor)") +
  theme_bw() +
  theme(legend.position="right")

sp2 <- plot_grid(digital_sp, regional_sp, language_sp,
          ncol = 1,
          align = "v",
          labels = LETTERS[1:3])

cor.test(scatterplot_tbl$MeanPR, scatterplot_tbl$ATV, method = "spearman") # 0.276
cor.test(scatterplot_tbl$MeanPR, scatterplot_tbl$ATV, method = "kendall") # 0.202

# mann whitney 
wilcox.test(ATV ~ Type, data = scatterplot_tbl, paired = FALSE)

#--------------------
wilcox.test(ATV ~ Region, data = scatterplot_tbl[scatterplot_tbl$Region %in% c("Regional", "National"),], paired = FALSE)

wilcox.test(ATV ~ Region, data = scatterplot_tbl[scatterplot_tbl$Region %in% c("Regional", "International"),], paired = FALSE)

wilcox.test(ATV ~ Region, data = scatterplot_tbl[scatterplot_tbl$Region %in% c("National", "International"),], paired = FALSE)

wilcox.test(scatterplot_tbl[scatterplot_tbl$Region == "International",]$ATV, scatterplot_tbl[scatterplot_tbl$Region == "National",]$ATV)

#--------------------
scatterplot_tbl_temp <- scatterplot_tbl %>%
  mutate(Language = ifelse(Language == "Both", "English", "Vernacular"))

wilcox.test(ATV ~ Language, data = scatterplot_tbl_temp, paired = FALSE)

scatterplot_tbl_temp <- scatterplot_tbl %>%
  mutate(Language = ifelse(Language == "Both", "Vernacular", "English"))

wilcox.test(scatterplot_tbl[scatterplot_tbl$Language == "English",]$ATV, scatterplot_tbl[scatterplot_tbl$Language == "Vernacular",]$ATV)




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

ordered_months = read_csv("03_Auxiliary/Fall 19/ordered_months.csv")

ordered_months %>%
  rename(Month = month) -> ordered_months

KM_ATV_master_df_common_breakdown <- KM_ATV_master_df_common %>%
  merge(media.breakdown) %>%
  mutate(Region = paste0(Indian, English)) %>%
  mutate(Region = ifelse(Region == "YN", "Regional",
                         ifelse(Region == "YY", "National", "International"))) %>%
  mutate(English = ifelse(English %in% c("Y", "B"), "English", "Vernacular")) %>%
  rename(Language = English) %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy")) %>%
  rename(Type = Digital) %>%
  merge(ordered_months)

# regional vs national vs international

KM_regional_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Region) %>%
  summarize(MeanATV = mean(ATV),
            MedianATV = median(ATV))

regional_mean_atv_trends_p <- ggplot(KM_regional_ATV_trends, aes(x=n, y=MeanATV)) +
  geom_point(color = "black") +
  facet_grid(~Region) +
  geom_smooth(method = "lm") +
  labs(x = "Month", y = expression(atop("Mean Average Time Spent", paste("per Visitor per Month (minutes)")))) +
  theme_bw()

regional_median_atv_trends_p <- ggplot(KM_regional_ATV_trends, aes(x=n, y=MedianATV)) +
  geom_point(color = "black") +
  facet_grid(~Region) +
  geom_smooth(method = "lm") +
  labs(x = "Month", y = expression(atop("Median Average Time Spent", paste("per Visitor per Month (minutes)")))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 14))

# model
# regional
regional_median_lm <- lm(MedianATV ~ n * Region, KM_regional_ATV_trends)

regional_region_median_lm <- lm(MedianATV ~ n, KM_regional_ATV_trends[KM_regional_ATV_trends$Region == "Regional",])
national_region_median_lm <- lm(MedianATV ~ n, KM_regional_ATV_trends[KM_regional_ATV_trends$Region == "National",])
international_region_median_lm <- lm(MedianATV ~ n, KM_regional_ATV_trends[KM_regional_ATV_trends$Region == "International",])

f <- function(x) format(round(x, 3), big.mark=",")

modelsummary(list("Median ATV (International)" = international_region_median_lm, "Median ATV (National)" = national_region_median_lm, "Median ATV (Regional)" = regional_region_median_lm),
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
             title = "OLS slope of median avg. time spent per visitor of regional, national, and international media over time"
)

modelsummary(list("Median ATV (International)" = international_region_median_lm, "Median ATV (National)" = national_region_median_lm, "Median ATV (Regional)" = regional_region_median_lm),
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
             title = "OLS slope of median avg. time spent per visitor on regional, national, and international media over time",
             output = "06_LaTeXTables/regional_atv_trends.tex"
)

# English vs Vernavular

KM_language_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Language) %>%
  summarize(MeanATV = mean(ATV),
            MedianATV = median(ATV))

language_atv_mean_trends_p <- ggplot(KM_language_ATV_trends, aes(x=n, y=MeanATV)) +
  geom_point(color = "black") +
  facet_grid(~Language) +
  geom_smooth(method = "lm") +
  labs(x = "Month", y = expression(atop("Mean Average Time Spent", paste("per Visitor per Month (minutes)")))) +
  theme_bw()

language_atv_median_trends_p <- ggplot(KM_language_ATV_trends, aes(x=n, y=MedianATV)) +
  geom_point(color = "black") +
  facet_grid(~Language) +
  geom_smooth(method = "lm") +
  labs(x = "Month", y = expression(atop("Median Average Time Spent", paste("per Visitor per Month (minutes)")))) +
  theme_bw()+
  theme(strip.text.x = element_text(size = 14))

# model

language_median_lm <- lm(MedianATV ~ n * Language, KM_language_ATV_trends)

english_language_median_lm <- lm(MedianATV ~ n, KM_language_ATV_trends[KM_language_ATV_trends$Language == "English",])
vernacular_language_median_lm <- lm(MedianATV ~ n, KM_language_ATV_trends[KM_language_ATV_trends$Language == "Vernacular",])

modelsummary(list("Median ATV (English)" = english_language_median_lm, "Median ATV (Vernacular)" = vernacular_language_median_lm),
             fmt = 5,
             stars = c('*' = 0.05, '**' = 0.025, '***' = 0.005),
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             notes = "p-values adjusted for multiple (2) comparisons",
             title = "OLS slope of median avg. time spent per visitor of regional, national, and international media over time"
)

modelsummary(list("Median ATV (English)" = english_language_median_lm, "Median ATV (Vernacular)" = vernacular_language_median_lm),
             fmt = 5,
             stars = c('*' = 0.05, '**' = 0.025, '***' = 0.005),
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             notes = "p-values adjusted for multiple (2) comparisons",
             title = "OLS slope of median avg. time spent on regional, national, and international media over time",
             output = "06_LaTeXTables/language_atv_trends.tex"
)

# Digital-born vs Legacy

KM_type_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  group_by(n, Type) %>%
  summarize(MeanATV = mean(ATV),
            MedianATV = median(ATV))

type_atv_mean_trends_p <- ggplot(KM_type_ATV_trends, aes(x=n, y=MeanATV)) +
  geom_point(color = "black") +
  facet_grid(~Type) +
  geom_smooth(method = "lm") +
  labs(x = "Month", y = expression(atop("Mean Average Time Spent", paste("per Visitor per Month (minutes)")))) +
  theme_bw()

type_atv_median_trends_p <- ggplot(KM_type_ATV_trends, aes(x=n, y=MedianATV)) +
  geom_point(color = "black") +
  facet_grid(~Type) +
  geom_smooth(method = "lm") +
  labs(x = "Month", y = expression(atop("Median Average Time Spent", paste("per Visitor per Month (minutes)")))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 14))

p4 <- plot_grid(type_atv_median_trends_p, regional_median_atv_trends_p, language_atv_median_trends_p, align = "v", labels = LETTERS[1:3], ncol = 1)

# model

digital_median_lm <- lm(MedianATV ~ n * Type, data = KM_type_ATV_trends)

digitalborn_median_lm <- lm(MedianATV ~ n, data = KM_type_ATV_trends[KM_type_ATV_trends$Type == "Digital-born",])

legacy_median_lm <- lm(MedianATV ~ n, data = KM_type_ATV_trends[KM_type_ATV_trends$Type == "Legacy",])

modelsummary(list("Median ATV (Digital-born)" = digitalborn_median_lm, "Median ATV (Legacy)" = legacy_median_lm),
             fmt = 5,
             stars = c('*' = 0.05, '**' = 0.025, '***' = 0.005),
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             title = "OLS slope of median avg. time spent per visitor on digital-born and legacy media over time",
             notes = "p-values adjusted for multiple (2) comparisons"
)

modelsummary(list("Median ATV (Digital-born)" = digitalborn_median_lm, "Median ATV (Legacy)" = legacy_median_lm),
             fmt = 5,
             stars = c('*' = 0.05, '**' = 0.025, '***' = 0.005),
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             title = "OLS slope of median avg. time spent per visitor on digital-born and legacy media over time",
             notes = "p-values adjusted for multiple (2) comparisons",
             output = "06_LaTeXTables/digital_atv_trends.tex"
)



# Indian digital-born vs Legacy

KM_i_type_ATV_trends <- KM_ATV_master_df_common_breakdown %>%
  filter(Region %in% c("Regional", "National")) %>%
  group_by(n, Type) %>%
  summarize(MeanATV = mean(ATV))

indian_type_atv_trends_p <- ggplot(KM_i_type_ATV_trends, aes(x=n, y=MeanATV)) +
  geom_point(color = "black") +
  facet_grid(~Type) +
  geom_smooth(method = "lm") +
  labs(x = "Month", y = expression(atop("Mean Average Time Spent", paste("per Visitor per Month (minutes)")))) +
  theme_bw()

# model

for(t in unique(KM_i_type_ATV_trends$Type)) {
  print(t)
  lm(MeanATV ~ n, data = KM_i_type_ATV_trends[KM_i_type_ATV_trends$Type == t,]) %>%
    summary() %>%
    print()
}

lm(MeanATV ~ n + Type, data = KM_i_type_ATV_trends) %>%
  summary() %>%
  print()

# robust
felm(ATV ~ n + Type | 0 | 0 | Media, KM_ATV_master_df_common_breakdown %>%
       filter(Region %in% c("Regional", "National"))) %>%
  summary() %>%
  print()

p4 <- plot_grid(regional_p, language_p, digital_p, align = "v", labels = c("A", "B", "C"), ncol = 1)
p5 <- plot_grid(regional_atv_trends_p, language_atv_trends_p, type_atv_trends_p, labels = c("A", "B", "C"), ncol = 1)
