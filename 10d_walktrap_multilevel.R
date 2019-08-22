rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

options(scipen=999)

library(igraph)
library(ggplot2)
library(gridExtra)

#load("04_RData/03_filtered_networks.RData")
#load("04_RData/02_induced_networks.RData")
load("04_RData/04_master_networks.RData")

set.seed(42)

WT = cluster_walktrap(filtered.master.g, weights = E(filtered.master.g)$shared_audience)

comm_list = vector("list", length(WT))
for(i in 1:length(WT)) {
  node.ids = unlist(lapply(WT[[i]], FUN = function(x) {which(x == V(filtered.master.g)$name)}))
  comm = induced.subgraph(filtered.master.g, node.ids)
  comm_list[[i]] = comm
}

comm_list2_WT = vector("list")

k = 1
for(i in 1:length(comm_list)) {
  WT2 = walktrap.community(comm_list[[i]], weights = E(comm_list[[i]])$shared_audience)
  for(j in 1:length(WT2)) {
    print(k)
    print(sort(WT2[[j]]))
    comm_list2_WT[[k]] = WT2[[j]]
    print("--------------------------")
    k =  k+1
  }
}

interlevel_map = NULL
for(i in 1:length(comm_list2_WT)) {
  for(j in 1:length(comm_list)) {
    if(all(comm_list2_WT[[i]] %in% V(comm_list[[j]])$name)) {
      print(paste0("final community ", i, " is in original community ", j))
      cr = c(i, j)
      interlevel_map = rbind(interlevel_map, cr)
    }
    comm_list2_WT[[1]]
  }
}

interlevel_map = data.frame(interlevel_map)
rownames(interlevel_map) = NULL
names(interlevel_map) = c("final_comm", "orig_comm")

##############################################################

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

comm_list2_WT
comm_network_EL = NULL

k = 1
for(i in 1:length(comm_list2_WT)) {
  for(j in 1:length(comm_list2_WT)) {
    if (i < j) {
      w = get.edge.wt.communities(filtered.master.g, comm_list2_WT[[i]], comm_list2_WT[[j]])
      comm.edge.wt = c(i, j, w)
      comm_network_EL = rbind(comm_network_EL, comm.edge.wt)
      print(k)
      print(paste0("edge weight between ", i, " and ", j, " is ", w))
      print("------------------")
      k = k+1
    }
    j = j+1
  }
  i = i+1
}

comm_network_EL = data.frame(comm_network_EL)
rownames(comm_network_EL) = NULL
names(comm_network_EL) = c("comm1", "comm2", "weight")
comm_network_EL = comm_network_EL[comm_network_EL$weight != 0,]

write.csv(comm_network_EL, "03_Auxiliary/multilevel_community_EL.csv", row.names = F)
write.csv(interlevel_map, "03_Auxiliary/interlevel_map.csv", row.names = F)

save(comm_list, comm_list2_WT, file = "04_RData/06_multilevel_walktrap_communities.Rdata")
