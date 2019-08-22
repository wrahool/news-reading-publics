rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")


library(igraph)
library(ggplot2)
library(gridExtra)

#load("04_RData/03_filtered_networks.RData")
#load("04_RData/02_induced_networks.RData")
load("04_RData/04_master_networks.RData")

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

#SL = cluster_spinglass(filtered.master.g, weights = E(filtered.master.g)$shared_audience, spins = 20)
FG = cluster_fast_greedy(filtered.master.g, weights = E(filtered.master.g)$shared_audience)
L = cluster_louvain(filtered.master.g, weights = E(filtered.master.g)$shared_audience)
WT = cluster_walktrap(filtered.master.g, weights = E(filtered.master.g)$shared_audience)

#save(SL, FG, L, WT, file = "04_RData/05_clustering_results.RData")

#FG

comm_list = vector("list", length(FG))
for(i in 1:length(FG)) {
  node.ids = unlist(lapply(FG[[i]], FUN = function(x) {which(x == V(filtered.master.g)$name)}))
  comm = induced.subgraph(filtered.master.g, node.ids)
  comm_list[[i]] = comm
}


comm_list2_FG = vector("list")

k = 1
for(i in 1:length(comm_list)) {
  FG2 = cluster_fast_greedy(comm_list[[i]], weights = E(comm_list[[i]])$shared_audience)
  for(j in 1:length(FG2)) {
    print(k)
    print(sort(FG2[[j]]))
    comm_list2_FG[[k]] = FG2[[j]]
    print("--------------------------")
    k =  k+1
  }
}

comm_list2_FG
comm_network_EL = NULL

k = 1
for(i in 1:length(comm_list2_FG)) {
  for(j in 1:length(comm_list2_FG)) {
    if (i < j) {
      w = get.edge.wt.communities(filtered.master.g, comm_list2_FG[[i]], comm_list2_FG[[j]])
      comm.edge.wt = c(i, j, w)
      comm_network_EL = rbind(comm_network_EL, comm.edge.wt)
      print(k)
      print(w)
      print("------------------")
      k = k+1
    }
    j = j+1
  }
  i = i+1
}

names(comm_network_EL) = c("from", "to", "shared_audience")

lapply(comm_list2_FG, write, "03_Auxiliary/FG_comm.txt", append=TRUE, ncolumns=1000)

write.csv(comm_network_EL, "03_Auxiliary/FG_multilevel.csv", row.names = F)

#L

comm_list = vector("list", length(L))
for(i in 1:length(L)) {
  node.ids = unlist(lapply(L[[i]], FUN = function(x) {which(x == V(filtered.master.g)$name)}))
  comm = induced.subgraph(filtered.master.g, node.ids)
  comm_list[[i]] = comm
}


comm_list2_L = vector("list")

k = 1
for(i in 1:length(comm_list)) {
  L2 = cluster_fast_greedy(comm_list[[i]], weights = E(comm_list[[i]])$shared_audience)
  for(j in 1:length(L2)) {
    print(k)
    print(sort(L2[[j]]))
    comm_list2_L[[k]] = L2[[j]]
    print("--------------------------")
    k =  k+1
  }
}

comm_list2_L
comm_network_EL = NULL

k = 1
for(i in 1:length(comm_list2_L)) {
  for(j in 1:length(comm_list2_L)) {
    if (i < j) {
      w = get.edge.wt.communities(filtered.master.g, comm_list2_L[[i]], comm_list2_L[[j]])
      comm.edge.wt = c(i, j, w)
      comm_network_EL = rbind(comm_network_EL, comm.edge.wt)
      print(k)
      print(w)
      print("------------------")
      k = k+1
    }
    j = j+1
  }
  i = i+1
}

names(comm_network_EL) = c("from", "to", "shared_audience")

lapply(comm_list2_L, write, "03_Auxiliary/L_comm.txt", append=TRUE, ncolumns=1000)

write.csv(comm_network_EL, "03_Auxiliary/L_multilevel.csv", row.names = F)

#WT

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
      print(w)
      print("------------------")
      k = k+1
    }
    j = j+1
  }
  i = i+1
}

names(comm_network_EL) = c("from", "to", "shared_audience")
write.csv(comm_network_EL, "03_Auxiliary/WT_multilevel.csv", row.names = F)

lapply(comm_list2_WT, write, "03_Auxiliary/WT_comm.txt", append=TRUE, ncolumns=1000)

save(comm_list2_FG, comm_list2_L, comm_list2_WT, file = "04_RData/06_multilevel_comms.RData")



