rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(wesanderson)

load("04_RData/Fall 19/01_networks.RData")
load("04_RData/Fall 19/02_induced_networks.RData")

KM.master.df = read.csv("03_Auxiliary/Fall 19/km_master.csv", as.is = T)
months = read.csv("03_Auxiliary/Fall 19/months.csv", as.is = T)
months = months$months

deg.cent.df = vector("list", length(red_graphs_list))

for(i in 1:length(red_graphs_list)) 
  deg.cent.df[[i]] = centr_degree(red_graphs_list[[i]], normalized = T)


common.media = read.csv("03_Auxiliary/Fall 19/common_nodes.csv", as.is = T)

#create a dataframe with all possible "deg cent" and "perc reach" pairs.
dc_pr.df = NULL
for(m1 in common.media$Media) {
  for(m2 in months) {
    #find month index
    m2.index = which(m2 == months)
    
    #find vertex label index in the graph indexed by month index
    m1.index = which(m1 == V(red_graphs_list[[m2.index]])$name)
    
    #look up degree centrality
    dc = deg.cent.df[[m2.index]]$res[m1.index]
    
    #look up percentage reach
    pr = KM.master.df[KM.master.df$Month == m2 & KM.master.df$Media == m1,]$PercentReach
    
    #dc-pr tuple
    dc_pr = c(m1, m2, dc, pr)
    dc_pr.df = rbind(dc_pr.df, dc_pr)
  }
}

dc_pr.df = as.data.frame(dc_pr.df)
names(dc_pr.df) = c("Media", "Month", "DC", "PR")
row.names(dc_pr.df) = 1:nrow(dc_pr.df)

dc_pr.df <- dc_pr.df %>%
  mutate(DC = as.numeric(DC),
         PR = as.numeric(PR))

cor.test(dc_pr.df$DC, dc_pr.df$PR, method = "pearson")
cor.test(dc_pr.df$DC, dc_pr.df$PR, method = "spearman")
cor.test(dc_pr.df$DC, dc_pr.df$PR, method = "kendall")

#read in the media breakdown file
media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

#join the two tables
dc_pr.full.df = merge(dc_pr.df, media.breakdown, by.x = "Media", by.y = "Media")

dc_pr.full.df$DigitalSocial = paste(dc_pr.full.df$Digital, dc_pr.full.df$Social, sep = "_")

#################################################
#now calculate average DC and PR for each outlet and repeat

#average DC
avg.dc = tapply(dc_pr.full.df$DC, dc_pr.full.df$Media, FUN = function(x) {mean(x, na.rm = T)})
avg.dc.df = as.data.frame(cbind(names(avg.dc), avg.dc), row.names = F)
names(avg.dc.df) = c("Media", "avg.dc")

#average PR
avg.pr = tapply(dc_pr.full.df$PR, dc_pr.full.df$Media, FUN = function(x) {mean(x, na.rm = T)})
avg.pr.df = as.data.frame(cbind(names(avg.pr), avg.pr), row.names = F)
names(avg.pr.df) = c("Media", "avg.pr")

#merge
avg.dc.pr.df = merge(avg.dc.df, avg.pr.df, by.x = "Media", by.y = "Media")

#cast type
avg.dc.pr.df <- avg.dc.pr.df %>%
  mutate(avg.dc = as.numeric(avg.dc),
         avg.pr = as.numeric(avg.pr))

cor.test(avg.dc.pr.df$avg.dc, avg.dc.pr.df$avg.pr, method = "pearson")
cor.test(avg.dc.pr.df$avg.dc, avg.dc.pr.df$avg.pr, method = "spearman")
cor.test(avg.dc.pr.df$avg.dc, avg.dc.pr.df$avg.pr, method = "kendall")

media.breakdown$X = NULL
avg.dc.pr.full.df = merge(avg.dc.pr.df, media.breakdown, by.x = "Media", by.y = "Media")

pr_density <- ggplot(avg.dc.pr.full.df) +
  geom_density(aes(avg.pr)) +
  labs(x="Mean Monthly Reach (%)") +
  theme_bw()

avg.dc.pr.full.df <- avg.dc.pr.full.df %>%
  mutate(RegionalIndian = paste0(Regional, Indian)) %>%
  mutate(RegionalIndian = ifelse(RegionalIndian == "NN", "International",
                                 ifelse(RegionalIndian == "YY", "Vernacular", "National"))) %>%
  select(-Regional, -Indian) %>%
  rename(Region = RegionalIndian) %>%
  mutate(English = ifelse(English == "Y", "English",
                          ifelse(English == "N", "Vernacular", "Both"))) %>%
  rename(Language = English) %>%
  mutate(Digital = ifelse(Digital == "Y", "Digital-born", "Legacy")) %>%
  rename(Type = Digital)

#viz Regional vs National vs International
regional_p <- ggplot(avg.dc.pr.full.df, aes(x=log(avg.dc), y=log(avg.pr), color=Region)) + 
  geom_point(size = 2, shape = 16) +
  theme_bw()+
  scale_color_brewer(palette="Set1") +
  labs(x = "log of degree centrality", y = "log of percentage reach") +
  theme(legend.position="right")

#viz English vs Vernacular
english_p = ggplot(avg.dc.pr.full.df, aes(x=log(avg.dc), y=log(avg.pr), color=Language)) + 
  geom_point(size = 2, shape = 16) +
  theme_bw()+
  scale_color_manual(values=wes_palette(name="GrandBudapest1"))+
  labs(x = "log of degree centrality", y = "log of percentage reach") +
  theme(legend.position="right")

# viz Digital born vs legacy

digital_p = ggplot(avg.dc.pr.full.df, aes(x=log(avg.dc), y=log(avg.pr), color=Type)) + 
  geom_point(size = 2, shape = 16) +
  theme_bw()+
  scale_color_manual(values=wes_palette(name="Royal1"))+
  labs(x = "log of degree centrality", y = "log of percentage reach") +
  theme(legend.position="right")

p1 <- plot_grid(regional_p, english_p, digital_p, ncol=1, align="v", labels = c("A", "B", "C"))

