library(readr)
library(dplyr)
library(igraph)
library(ggplot2)
library(lfe)
library(lmtest)
library(cowplot)
library(stargazer)

setwd("C:\\Users\\Subhayan Mukerjee\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

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
  summarize(MeanPO = mean(PercentOveralap), MedianPO = median(PercentOveralap)) -> all_vernacular_to_national_tbl

v2n_mean_plot <- ggplot(all_vernacular_to_national_tbl, aes(x=n, y=MeanPO, color = Type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = expression(atop("Mean Percent Overlap of Vernacular Media", paste("with National Media (%)"))), col = "Type of National Media") +
  theme(legend.position="bottom")

v2n_median_plot <- ggplot(all_vernacular_to_national_tbl, aes(x=n, y=MedianPO, color = Type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = expression(atop("Median Percent Overlap of Vernacular Media", paste("with National Media (%)"))), col = "Type of National Media") +
  theme(legend.position="bottom")


v2n_median_lm <- lm(MedianPO ~ n * Type, all_vernacular_to_national_tbl)

v2n_db_median_lm <- lm(MedianPO ~ n, all_vernacular_to_national_tbl[all_vernacular_to_national_tbl$Type == "Digital-born",])
v2n_l_median_lm <- lm(MedianPO ~ n, all_vernacular_to_national_tbl[all_vernacular_to_national_tbl$Type == "Legacy",])

modelsummary(list("Median % Overlap with National Digital Born" = v2n_db_median_lm, "Median % Overlap with National Legacy" = v2n_l_median_lm),
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
             title = "OLS slope of median percent overlap of regional media and international media outlets over time"
)

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

v2n_felm <- felm(PercentOveralap ~ n + Type | 0 | 0 | VernacularMedia + OtherMedia, mobility_national)

latex_table_v2n_robust <- stargazer(v2n_felm,
                                    title = "OLS regression (with robust errors) of Percent Overlap against Time of Vernacular and National media outlets")




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
  summarize(MeanPO = mean(PercentOveralap),
            MedianPO = median(PercentOveralap)) -> all_vernacular_to_international_tbl

v2i_mean_plot <- ggplot(all_vernacular_to_international_tbl, aes(x=n, y=MeanPO, color = Type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = expression(atop("Mean Percent Overlap of Vernacular Media", paste("with International Media (%)"))), col = "Type of International Media") +
  theme(legend.position="bottom")

v2i_median_plot <- ggplot(all_vernacular_to_international_tbl, aes(x=n, y=MedianPO, color = Type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Month", y = expression(atop("Median Percent Overlap of Vernacular Media", paste("with International Media (%)"))), col = "Type of International Media") +
  theme(legend.position="bottom")

v2i_median_lm <- lm(MedianPO ~ n * Type, all_vernacular_to_international_tbl)

v2i_db_median_lm <- lm(MedianPO ~ n, all_vernacular_to_international_tbl[all_vernacular_to_international_tbl$Type == "Digital-born",])
v2i_l_median_lm <- lm(MedianPO ~ n, all_vernacular_to_international_tbl[all_vernacular_to_international_tbl$Type == "Legacy",])

modelsummary(list("Median % Overlap with International Digital Born" = v2i_db_median_lm, "Median % Overlap with International Legacy" = v2i_l_median_lm),
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
             title = "OLS slope of median percent overlap of regional media and international media outlets over time"
)

# both main models

modelsummary(list("Median % Overlap with National Media" = v2n_median_lm, "Median % Overlap with International Media" = v2i_median_lm),
             fmt = 5,
             stars = TRUE,
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)",
                          "n:TypeLegacy" = "n x Legacy"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             title = "OLS slope of median percent overlap of regional media and international media outlets over time",
             output = "06_LaTeXTables/aud_mob.tex"
)

modelsummary(list("Median % Overlap with National Media" = v2n_median_lm, "Median % Overlap with International Media" = v2i_median_lm),
             fmt = 5,
             stars = TRUE,
             align = "lll",
             gof_omit = "IC|Log|F",
             coef_map = c("(Intercept)" = "(Intercept)", 
                          "n" = "n (month)",
                          "n:TypeLegacy" = "n x Legacy"),
             gof_map = list(
               list("raw" = "r.squared", "clean" = "R-squared", "fmt" = f)
             ),
             title = "OLS slope of median percent overlap of regional media and international media outlets over time",
             output = "06_LaTeXTables/aud_mob.tex"
)

plot_grid(v2n_median_plot, v2i_median_plot,
          nrow = 1,
          labels = LETTERS[1:2])


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


v2i_felm <- felm(PercentOveralap ~ n + Type | 0 | 0 | VernacularMedia + OtherMedia, mobility_international)

save(v2n_mean_plot, v2n_median_plot, file = "04_RData/post-dissertation/aud_mob_v2n_ggplot.Rdata")
save(v2i_mean_plot, v2i_median_plot, file = "04_RData/post-dissertation/aud_mob_v2i_ggplot.Rdata")
