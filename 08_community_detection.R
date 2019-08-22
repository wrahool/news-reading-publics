#setwd("C:\\Users\\Subhayan\\Documents\\Python Workspace\\India")
rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")


library(igraph)
library(ggplot2)
library(gridExtra)

load("04_RData/03_filtered_networks.RData")
load("04_RData/02_induced_networks.RData")
load("04_RData/04_master_networks.RData")

test.g = filtered_graphs_list[[1]]

#community detection in the monthly networks

g = red_graphs_list[[1]]
fg = filtered_graphs_list[[1]]

gc = cluster_louvain(g, weights = E(g)$shared_audience)
fgc = cluster_louvain(fg, weights = E(fg)$shared_audience)

media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

clustering.df = as.data.frame(cbind(gc$names, gc$membership))
names(clustering.df) = c("Media", "cluster")
clustering.df.breakdown = merge(clustering.df, media.breakdown, all.x = T)

table(clustering.df.breakdown[clustering.df.breakdown$cluster == 1,]$Indian)
table(clustering.df.breakdown[clustering.df.breakdown$cluster == 2,]$Indian)
table(clustering.df.breakdown[clustering.df.breakdown$cluster == 3,]$Indian)
table(clustering.df.breakdown[clustering.df.breakdown$cluster == 4,]$Indian)

clustering.df.f = as.data.frame(cbind(fgc$names, fgc$membership))
names(clustering.df.f) = c("Media", "cluster")
clustering.df.f.breakdown = merge(clustering.df, media.breakdown, all.x = T)

table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 1,]$Indian)
table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 2,]$Indian)
table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 3,]$Indian)
table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 4,]$Indian)

table(clustering.df.breakdown[clustering.df.f.breakdown$cluster == 1,]$Language)
table(clustering.df.breakdown[clustering.df.breakdown$cluster == 2,]$Language)
table(clustering.df.breakdown[clustering.df.breakdown$cluster == 3,]$Language)
table(clustering.df.breakdown[clustering.df.breakdown$cluster == 4,]$Language)

#####################################################

#with master networks

master.g.c = cluster_louvain(master.g, weights = E(master.g)$shared_audience)
clustering.df = as.data.frame(cbind(master.g.c$names, master.g.c$membership))
names(clustering.df) = c("Media", "cluster")


#filtered.master.g.c = cluster_louvain(filtered.master.g, weights = E(filtered.master.g)$shared_audience)
filtered.master.g.c = cluster_fast_greedy(filtered.master.g, weights = E(filtered.master.g)$shared_audience)
#filtered.master.g.c = cluster_infomap(filtered.master.g, e.weights = E(filtered.master.g)$shared_audience)
filtered.master.g.c = cluster_spinglass(filtered.master.g, weights = E(filtered.master.g)$shared_audience)

clustering.df.f = as.data.frame(cbind(filtered.master.g.c$names, filtered.master.g.c$membership))
names(clustering.df.f) = c("Media", "cluster")


media.breakdown = read.csv("Auxiliary/all_media_breakdown.csv", as.is = T)

clustering.df.breakdown = merge(clustering.df, media.breakdown, all.x = T)
clustering.df.f.breakdown = merge(clustering.df.f, media.breakdown, all.x = T)

table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 1,]$Language)
table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 2,]$Language)
table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 3,]$Language)
table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 4,]$Language)
table(clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 5,]$Language)


clustering.df.f.breakdown[clustering.df.f.breakdown$cluster == 1 & 
                                  clustering.df.f.breakdown$Language == "BR",]$Media

SL = cluster_spinglass(filtered.master.g, weights = E(filtered.master.g)$shared_audience, spins = 20)
FG = cluster_fast_greedy(filtered.master.g, weights = E(filtered.master.g)$shared_audience)
L = cluster_louvain(filtered.master.g, weights = E(filtered.master.g)$shared_audience)
WT = cluster_walktrap(filtered.master.g, weights = E(filtered.master.g)$shared_audience)

save(SL, FG, L, WT, file = "04_RData/05_clustering_results.RData")


dendPlot(FG, mode = "hclust")

l1 = layout_with_fr(filtered.master.g)
l2 = layout_with_kk(filtered.master.g)
l3 = layout_with_lgl(filtered.master.g)
#l4 = layout_with_dh(filtered.master.g)
l5 = layout_with_graphopt(filtered.master.g)
l6 = layout_with_mds(filtered.master.g) 

plot(FG, filtered.master.g, vertex.label = NA, vertex.size = 1, layout = l1)

comm_list = vector("list", length(FG))
for(i in 1:length(L)) {
  node.ids = unlist(lapply(L[[i]], FUN = function(x) {which(x == V(filtered.master.g)$name)}))
  comm = induced.subgraph(filtered.master.g, node.ids)
  comm_list[[i]] = comm
}

comm = 1



