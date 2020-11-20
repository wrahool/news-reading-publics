library(trend)
library(tidyverse)
library(cowplot)
library(igraph)
library(ggplot2)

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

# regional trends

regional_trends <- media_master_breakdown %>%
  group_by(n, paste0(Regional, Indian)) %>%
  summarise(MeanPC = mean(PercentReach)) %>%
  ungroup() %>%
  rename(Region = 2) %>%
  mutate(Region = ifelse(Region == "NN", "International", ifelse(Region == "NY", "National", "Regional")))

# non-parametric slopes by type of Region
region_res <- tapply(regional_trends$MeanPC,
       regional_trends$Region,
       FUN = function(x) {
         x %>% 
           ts(frequency = 12, start = c(2014, 10)) %>%
           sens.slope(conf.level = 0.95)
       }
       )

nonp_regional_slopes <- sapply(region_res, FUN = function(x) {
  c(ci_lower = x$conf.int[1], x$estimate, ci_upper = x$conf.int[2])}) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Type")

regional_plot <- ggplot(nonp_regional_slopes, aes(x = Type, y = Sen.s.slope)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), size = 1, width = 0.2) +
  labs(x = "Media Type", y = "Sen's slope for Temporal Change in Percent Reach") +
  lims(y = c(-0.01, 0.01)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw()

# digital trends
digital_trends <- media_master_breakdown %>%
  group_by(n, Digital) %>%
  summarise(MeanPC = mean(PercentReach)) %>%
  ungroup() %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy"))

# non-parametric slopes by type of digital
digital_res <- tapply(digital_trends$MeanPC,
                      digital_trends$Digital,
                      FUN = function(x) {
                        x %>%
                          ts(frequency = 12, start = c(2014, 10)) %>%
                          sens.slope(conf.level = 0.95)
                      }
                      )

nonp_digital_slopes <- sapply(digital_res, FUN = function(x) {
  c(ci_lower = x$conf.int[1], x$estimate, ci_upper = x$conf.int[2])}) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Type")

digital_plot <- ggplot(nonp_digital_slopes, aes(x = Type, y = Sen.s.slope)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), size = 1, width = 0.2) +
  labs(x = "Media Type", y = "Sen's slope for Temporal Change in Percent Reach") +
  lims(y = c(-0.01, 0.01)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw()


#english trends
english_trends <- media_master_breakdown %>%
  mutate(English = ifelse(English == "N", "N", "Y")) %>%
  group_by(n, English) %>%
  summarise(MeanPC = mean(PercentReach)) %>%
  ungroup() %>%
  mutate(English = ifelse(English == "Y", "English", "Vernacular"))

# non-parametric slopes by type of english
english_res <- tapply(english_trends$MeanPC,
                      english_trends$English,
                      FUN = function(x) {
                        x %>%
                          ts(frequency = 12, start = c(2014, 10)) %>%
                          sens.slope(conf.level = 0.95)
                      }
                      )

nonp_english_slopes <- sapply(english_res, FUN = function(x) {
  c(ci_lower = x$conf.int[1], x$estimate, ci_upper = x$conf.int[2])}) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Type")

english_plot <- ggplot(nonp_english_slopes, aes(x = Type, y = Sen.s.slope)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), size = 1, width = 0.2) +
  labs(x = "Media Type", y = "Sen's slope for Temporal Change in Percent Reach") +
  lims(y = c(-0.01, 0.01)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw()

# non-parametric slopes for audience mobility (vernacular to national)

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

vernacular_to_national_mobility_res <- tapply(all_vernacular_to_national_tbl$MeanPO,
       all_vernacular_to_national_tbl$Type,
       FUN = function(x) {
         x %>%
           ts(frequency = 12, start = c(2014, 10)) %>%
           sens.slope(conf.level = 0.95)
       })

nonp_v2n_slopes <- sapply(vernacular_to_national_mobility_res, FUN = function(x) {
  c(ci_lower = x$conf.int[1], x$estimate, ci_upper = x$conf.int[2])}) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Type")

v2n_plot <- ggplot(nonp_v2n_slopes, aes(x = Type, y = Sen.s.slope)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), size = 1, width = 0.2) +
  labs(x = "Media Type", y = "Sen's slope for Audience Mobility to National Media") +
  lims(y = c(0, 0.22)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw()

# non parametric slopes of audience mobility (vernacular to international)

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

vernacular_to_international_mobility_res <- tapply(all_vernacular_to_international_tbl$MeanPO,
                                                   all_vernacular_to_international_tbl$Type,
                                                   FUN = function(x) {
                                                     x %>%
                                                       ts(frequency = 12, start = c(2014, 10)) %>%
                                                       sens.slope(conf.level = 0.95)
                                                   })

nonp_v2i_slopes <- sapply(vernacular_to_international_mobility_res, FUN = function(x) {
  c(ci_lower = x$conf.int[1], x$estimate, ci_upper = x$conf.int[2])}) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("Type")

v2i_plot <- ggplot(nonp_v2i_slopes, aes(x = Type, y = Sen.s.slope)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), size = 1, width = 0.2) +
  labs(x = "Media Type", y = "Sen's slope for Audience Mobility to International Media") +
  lims(y = c(0, 0.22)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw()

sp1 <- plot_grid(regional_plot, digital_plot, english_plot, ncol = 1, align = "v", labels = c("A", "B", "C"))
sp2 <- plot_grid(v2n_plot, v2i_plot, ncol = 1, align = "v", labels = c("A", "B"))

