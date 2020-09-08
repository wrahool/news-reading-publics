rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

##################################
# Prepare data for visualization #
##################################

KM.master.df = read.csv("03_Auxiliary/Fall 19/km_master.csv", as.is = T)
all.media.breakdown = read.csv("03_Auxiliary/Fall 19/common_media_breakdown.csv", as.is = T)

#regular WT
load("04_RData/Fall 19/WT.Rdata")

res_df = NULL
for(i in 1:max(WT$membership)) {
  outlets = WT$names[WT$membership == i]
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
for(i in 1:max(WT2$membership)) {
  outlets = WT2$names[WT2$membership == i]
  n_outlets = length(outlets)
  km = KM.master.df[KM.master.df$Media %in% outlets,]
  mean_pc = mean(tapply(km$PercentReach, km$Media, mean))
  median_PC = median(tapply(km$PercentReach, km$Media, median))
  mean_UV = mean(tapply(km$UV, km$Media, mean))
  median_UV = median(tapply(km$UV, km$Media, median))
  
  regional_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                                all.media.breakdown$Regional == "Y",]) * 100 / length(outlets)
  digital_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                               all.media.breakdown$Digital == "Y",]) * 100 / length(outlets)
  vernacular_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                                  all.media.breakdown$English == "N",]) * 100 / length(outlets)
  indian_percent = nrow(all.media.breakdown[all.media.breakdown$Media %in% outlets &
                                              all.media.breakdown$Indian == "Y",]) * 100 / length(outlets)
  
  lignuistic_diversity = length(unique(all.media.breakdown[all.media.breakdown$Media %in% outlets,]$State))
  
  res = data.frame(t(c(i, n_outlets, mean_pc, mean_UV, median_PC, median_UV,
                       regional_percent, digital_percent, vernacular_percent, indian_percent,
                       lignuistic_diversity)))
  
  res_df = rbind(res_df, res)
}

names(res_df) = c("comm", "n", "mean_pc", "mean_UV", "median_PC", "median_UV",  "reg_percent", "dig_percent", "ver_percent", "ind_percent", "ling_div")

write.csv(res_df, "03_Auxiliary/resolution_walktrap_community_features.csv", row.names = F)

##################################
#          visualization         #
##################################
library(igraph)

#regular WT

# nodes = read.csv("03_Auxiliary/simple_walktrap_community_features.csv", as.is = TRUE)
# links = read.csv("03_Auxiliary/simple_WT_community_network.csv", as.is = TRUE)

type = "simple"

nodes = read.csv(paste0("03_Auxiliary/Fall 19/", type,"_walktrap_community_features.csv"), as.is = TRUE)
links = read.csv(paste0("03_Auxiliary/Fall 19/", type,"_WT_community_network.csv"), as.is = TRUE)


names(links)[1:2] = c("from", "to")

net = graph_from_data_frame(d=links, vertices=nodes, directed=F) 

V(net)$size <- V(net)$n * 0.7
E(net)$width <- E(net)$weight/60000

colfunc <- colorRampPalette(c("orangered", "orangered4"))
V(net)$color <- colfunc(11)[round(V(net)$mean_UV * 10 / max(V(net)$mean_UV))+1]

# l <- layout_with_fr(net)
load("03_Auxiliary/Fall 19/simple_WT_layout.Rdata")

plot.igraph(net, edge.arrow.size=0.1,
     vertex.frame.color="#ffffff",
     vertex.label=V(net)$comm,
     vertex.label.color="black",
     layout = l) 

plot.igraph(net,
            vertex.frame.color="#ffffff",
            vertex.label=NA,
            layout = l,
            frame = T,
            margin = c(-0.3,-0.3,-0.3,-0.3))

# save(l, file = "03_Auxiliary/simple_WT_layout.Rdata")

##################################
library(igraph)
type = "resolution"

nodes = read.csv(paste0("03_Auxiliary/", type,"_walktrap_community_features.csv"), as.is = TRUE)
links = read.csv(paste0("03_Auxiliary/", type,"_WT_community_network.csv"), as.is = TRUE)

names(links)[1:2] = c("from", "to")

net = graph_from_data_frame(d=links, vertices=nodes, directed=F) 

V(net)$size <- V(net)$n * 0.7
E(net)$width <- E(net)$weight/100000

colfunc <- colorRampPalette(c("orangered", "orangered4"))
V(net)$color <- colfunc(16)[round(V(net)$mean_UV * 10 / max(V(net)$mean_UV))+1]

#l <- layout_with_fr(net)
load("03_Auxiliary/resolution_WT_layout.Rdata")

l <- norm_coords(l, ymin=-0.07, ymax=0.07, xmin=-0.1, xmax=0.1)

#par(mfrow=c(2,2), mar=c(0,0,0,0))
dev.off()
# dev.new()

plot(net, edge.arrow.size=.2,
     # edge.color="black",
     vertex.frame.color="#ffffff",
    vertex.label=V(net)$comm,
     #vertex.label=NA,
     vertex.label.color="black",
     layout = l*15,
     rescale = FALSE) 

#save(l, file = "03_Auxiliary/resolution_WT_layout.Rdata")

# core only
dev.off()
core_vertices = c(1:4, 6:8)
net_core = induced_subgraph(net, vids = core_vertices)

core_l = l[core_vertices,]
core_l <- norm_coords(core_l, ymin=-0.2, ymax=0.2, xmin=-0.2, xmax=0.2)

V(net_core)$size <- V(net_core)$n

# core_l[3,1] = 0.022
# core_l[3,2] = 0.0045

# dev.new()
plot(net_core, edge.arrow.size=.2,
     # edge.color="black",
     vertex.frame.color="#ffffff",
     #vertex.label=V(net)$comm,
     vertex.label=NA,
     vertex.label.color="black",
     layout = core_l*15,
     rescale = TRUE) 
