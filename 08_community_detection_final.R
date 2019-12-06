rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")


library(igraph)
library(ggplot2)
library(gridExtra)

#load("04_RData/03_filtered_networks.RData")
#load("04_RData/02_induced_networks.RData")
load("04_RData/04_master_networks.RData")

#function to get edge weights between communities
get.edge.wt.communities = function(g, c1, c2) {
  wt = 0
  for(v1 in c1) {
    for(v2 in c2) {
      if (length(E(g)[v1 %--% v2]$shared_audience) != 0) {
        wt = wt + E(g)[v1 %--% v2]$shared_audience
      }
    }
  }
  return(wt)
}

#multi-level walktrap without resolution parameter
set.seed(42)
WT = cluster_walktrap(filtered.master.g, weights = E(filtered.master.g)$shared_audience)
modularity(filtered.master.g, membership(WT))

save(WT, file = "04_RData/WT.Rdata")
# 
# comm_list = vector("list", length(WT))
# for(i in 1:length(WT)) {
#   node.ids = unlist(lapply(WT[[i]], FUN = function(x) {which(x == V(filtered.master.g)$name)}))
#   comm = induced.subgraph(filtered.master.g, node.ids)
#   comm_list[[i]] = comm
# }
# 
# comm_list2_WT = vector("list")
# 
# k = 1
# for(i in 1:length(comm_list)) {
#   WT2 = walktrap.community(comm_list[[i]], weights = E(comm_list[[i]])$shared_audience)
#   for(j in 1:length(WT2)) {
#     print(k)
#     print(sort(WT2[[j]]))
#     comm_list2_WT[[k]] = WT2[[j]]
#     print("--------------------------")
#     k =  k+1
#   }
# }

#walk-trap with resolution parameter
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
modularity(g, membership(WT2))

save(WT2, file = "04_RData/WT2.Rdata")


#Arenas
resolution_df = NULL
for(i in seq(from = 1, to = 10000, by = 50)){
  print(i)
  g = filtered.master.g
  g[from=V(g), to=V(g)] = 1
  for(v in V(g)$name) {
    E(g)[v %--% v]$shared_audience = i
  }
  set.seed(42)
  WT3 = cluster_walktrap(g, weights = E(g)$shared_audience)
  n_c = max(WT3$membership)
  
  row = c(i, n_c)
  resolution_df = rbind(resolution_df, row)
}

i = 7401

g = filtered.master.g
g[from=V(g), to=V(g)] = 1
for(v in V(g)$name) {
  E(g)[v %--% v]$shared_audience = i
}
set.seed(42)
WT3 = cluster_walktrap(g, weights = E(g)$shared_audience)

save(WT3, file = "04_RData/WT3.Rdata")


#build community network

#regular Walktrap
comm_g1_EL = NULL

for(i in 1:length(WT)) {
  for(j in 1:length(WT)) {
    if(i < j) {
      
      print(paste0(i, " and ", j))
      row = data.frame(i, j, get.edge.wt.communities(filtered.master.g, WT[[i]], WT[[j]]))
      names(row) = c("comm1", "comm2", "weight")
      comm_g1_EL = rbind(comm_g1_EL, row)
      print("------------------")
    }
  }
}

#walktrap with resolution parameter
comm_g2_EL = NULL

for(i in 1:length(WT2)) {
  for(j in 1:length(WT2)) {
    if(i < j) {
      
      print(paste0(i, " and ", j))
      row = data.frame(i, j, get.edge.wt.communities(filtered.master.g, WT2[[i]], WT2[[j]]))
      names(row) = c("comm1", "comm2", "weight")
      comm_g2_EL = rbind(comm_g2_EL, row)
      print("------------------")
    }
  }
}

options(scipen=999)

write.csv(comm_g1_EL, "03_Auxiliary/simple_WT_community_network.csv", row.names = F)
write.csv(comm_g2_EL, "03_Auxiliary/resolution_WT_community_network.csv", row.names = F)
