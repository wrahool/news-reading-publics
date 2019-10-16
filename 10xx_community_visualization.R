rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)
all.media.breakdown = read.csv("03_Auxiliary/common_media_breakdown.csv", as.is = T)

#regular WT
load("04_RData/WT.Rdata")

res_df = NULL
for(i in 1:length(WT)) {
  outlets = WT[[i]]
  n_outlets = length(outlets)
  km = KM.master.df[KM.master.df$Media %in% outlets,]
  mean_pc = mean(tapply(km$PercentReach, km$Media, mean))
  mean_UV = mean(tapply(km$UV, km$Media, mean))
  
  regional_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                               all.media.breakdown$Regional == "Y",]) * 100 / length(outlets)
  digital_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                                all.media.breakdown$Digital == "Y",]) * 100 / length(outlets)
  vernacular_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                               all.media.breakdown$English == "N",]) * 100 / length(outlets)
  indian_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                               all.media.breakdown$Indian == "Y",]) * 100 / length(outlets)
  
  lignuistic_diversity = length(unique(all.media.breakdown[all.media.breakdown$Media %in% outlets,]$State))
  
  res = data.frame(t(c(i, n_outlets, mean_pc, mean_UV,
                       regional_percent, digital_percent, vernacular_percent, indian_percent,
                       lignuistic_diversity)))
  
  res_df = rbind(res_df, res)
}

names(res_df) = c("comm", "n", "mean_pc", "mean_UV", "reg_percent", "dig_percent", "ver_percent", "ind_percent", "ling_div")

write.csv(res_df, "03_Auxiliary/simple_walktrap_community_features.csv", row.names = F)

#WT with resolution parameter
load("04_RData/WT2.Rdata")

res_df = NULL
for(i in 1:length(WT2)) {
  outlets = WT2[[i]]
  n_outlets = length(outlets)
  km = KM.master.df[KM.master.df$Media %in% outlets,]
  mean_pc = mean(tapply(km$PercentReach, km$Media, mean))
  mean_UV = mean(tapply(km$UV, km$Media, mean))
  
  regional_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                                all.media.breakdown$Regional == "Y",]) * 100 / length(outlets)
  digital_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                               all.media.breakdown$Digital == "Y",]) * 100 / length(outlets)
  vernacular_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                                  all.media.breakdown$English == "N",]) * 100 / length(outlets)
  indian_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                              all.media.breakdown$Indian == "Y",]) * 100 / length(outlets)
  
  lignuistic_diversity = length(unique(all.media.breakdown[all.media.breakdown$Media %in% outlets,]$State))
  
  res = data.frame(t(c(i, n_outlets, mean_pc, mean_UV,
                       regional_percent, digital_percent, vernacular_percent, indian_percent,
                       lignuistic_diversity)))
  
  res_df = rbind(res_df, res)
}

names(res_df) = c("comm", "n", "mean_pc", "mean_UV", "reg_percent", "dig_percent", "ver_percent", "ind_percent", "ling_div")

write.csv(res_df, "03_Auxiliary/resolution_walktrap_community_features.csv", row.names = F)
