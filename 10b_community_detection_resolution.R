setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")


library(igraph)
library(ggplot2)
library(gridExtra)

#load("04_RData/03_filtered_networks.RData")
#load("04_RData/02_induced_networks.RData")
load("04_RData/04_master_networks.RData")

#DON'T RUN ANY OF THIS
WT = cluster_walktrap(filtered.master.g, weights = E(WT)$shared_audience)

#resolution analysis

sl_vec = seq(100, 7500, by = 10)
sl_df = NULL

g = filtered.master.g
for(sl in sl_vec) {
  print(sl)
  g[from=V(g),to=V(g)] = 1
  for(v in V(g)$name) {
    E(g)[v %--% v]$shared_audience = sl
  }
  
  WT_sl = cluster_walktrap(g, weights = E(g)$shared_audience)
  sl_row = c(sl, length(WT_sl))
  sl_df = rbind(sl_df, sl_row)
}

################
#RUN THIS ONLY
#set resolution as audience reach
g = filtered.master.g
g[from=V(g), to=V(g)] = 1

KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)

KM.master.total.df = tapply(KM.master.df$UV, KM.master.df$Media, sum)
KM.master.total.df = as.data.frame(cbind(names(KM.master.total.df), KM.master.total.df), row.names = F)
names(KM.master.total.df) = c("media", "total.uv")

for(v in V(g)$name) {
  E(g)[v %--% v]$shared_audience = KM.master.total.df[KM.master.total.df$media == v,]$total.uv
}
set.seed(42)
WT2 = cluster_walktrap(g, weights = E(g)$shared_audience)

save(WT2, file = "03_Auxiliary/communities.RData")

#multi level

i = 3
WT2[[i]]
node_ids = which(WT2$membership == i)
V(g)$name[which(WT2$membership == i)]
g2 = induced.subgraph(g, node_ids)  

WT3 = cluster_walktrap(g2, weights = E(g2)$shared_audience)
print(WT3)
