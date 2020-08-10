rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(tidyverse)

#barplot

newspapers_tbl = read_csv("../../newspapers_IRS_data.csv")

positions <- newspapers_tbl %>% pull(Newspaper)

newspapers_tbl %>%
  gather(Year, Readership, -Newspaper, -Type) %>%
  mutate(Newspaper = as.factor(Newspaper))-> newspapers_tbl

newspapers_tbl %>%
  mutate(Newspaper = fct_relevel(Newspaper, positions)) -> newspapers_tbl

newspapers_tbl %>%
  group_by(Type) %>%
  summarize(meanReadership = mean(Readership)) -> mean_readership


# ggplot(data = newspapers_tbl, aes(x=Newspaper, y=Readership)) +
#   geom_bar(stat="identity", aes(fill=Type)) +
#   facet_wrap(~gsub("IRS", "", Year), nrow = 2) +
#   theme_bw() +
#   scale_fill_manual("legend", values = c("English" = "red4",
#                                          "Hindi" = "navyblue",
#                                          "Other" = "darkorange2")) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

# boxplot

newspapers_tbl = read_csv("../../newspapers_IRS_data.csv")

newspapers_tbl %>%
  gather(Year, Readership, -Newspaper, -Type) -> newspapers_tbl

p1 = ggplot(data = newspapers_tbl, aes(x=Type, y=log(Readership), fill = Type)) +
  geom_boxplot() +
  #geom_point(position=position_jitterdodge()) +
  ylab("Readership in thousands") +
  facet_wrap(.~gsub("IRS", "", Year)) +
  theme_bw() +
  scale_fill_manual("legend", values = c("English" = "firebrick",
                                         "Hindi" = "dodgerblue",
                                         "Other" = "darkorange2"))+
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position="none")+
  theme(text = element_text(size=20))


# read community as nodes data to compare with newspaper
df = read_csv("03_Auxiliary/resolution_walktrap_community_features.csv")
load("04_RData/WT2.Rdata")

WT2[[2]]

nat_english_outlets = WT2[[4]]
north_indian_outlets = WT2[[2]]
other_language_outlets = c(WT2[[1]], WT2[[3]], WT2[[5]], WT2[[7]], WT2[[8]], WT2[[9]], WT2[[10]])

reqd_outlets = c(nat_english_outlets, north_indian_outlets, other_language_outlets)

KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)

nat_english_KM = KM.master.df[KM.master.df$Media %in% nat_english_outlets,]

nat_english_KM %>%
  group_by(Media) %>%
  summarize(MeanUV = mean(UV)) -> nat_english_KM

nat_english_KM$type = "English"

north_indian_KM = KM.master.df[KM.master.df$Media %in% north_indian_outlets,]

north_indian_KM %>%
  group_by(Media) %>%
  summarize(MeanUV = mean(UV)) -> north_indian_KM

north_indian_KM$type = "Hindi + Northern languages"

other_KM = KM.master.df[KM.master.df$Media %in% other_language_outlets,]
other_KM %>%
  group_by(Media) %>%
  summarize(MeanUV = mean(UV)) -> other_KM

other_KM$type = "Other languages"

reqd_df = rbind(nat_english_KM, north_indian_KM, other_KM)

p2 = ggplot(data = reqd_df, aes(x=type, y=MeanUV, fill = type)) +
  geom_boxplot() +
  # geom_point(position=position_jitterdodge()) +
  theme_bw() +
  scale_fill_manual("legend", values = c("English" = "firebrick",
                                         "Hindi + Northern languages" = "dodgerblue",
                                         "Other languages" = "darkorange2"))+
  theme(axis.text=element_text(size=13),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.position="none")

ggarrange(p1, p2)
