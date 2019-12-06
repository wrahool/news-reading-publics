rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)

load("04_RData/01_networks.RData")

#turn this to T if you want to see the plots
visualize = T

#create an empty matrix of 9 columns.
m = matrix(NA,length(graphs_list),9)
#detach("package:sna", unload=TRUE)

#get network parameters

months = read.csv("03_Auxiliary/months.csv", as.is = T)
months = months$months

for(i in 1:length(graphs_list)){
  m[i,1] = months[i]
  m[i,2] = vcount(graphs_list[[i]])
  m[i,3] = ecount(graphs_list[[i]])
  m[i,4] = mean(degree(graphs_list[[i]],mode="all"))
  m[i,5] = sd(degree(graphs_list[[i]],mode="all"))
  m[i,6] = mean(E(graphs_list[[i]])$shared_audience)
  m[i,7] = sd(E(graphs_list[[i]])$shared_audience)
  m[i,8] = centr_degree(graphs_list[[i]],mode="all")$centralization
  m[i,9] = edge_density(graphs_list[[i]])
}

colnames(m) = c("month", "nodes", "edges","mean.degree", "sd.degree", 
               "mean.xvisiting", "sd.xvisiting", "centralization", "density")

m = as.data.frame(m)

#change data types

m$month = as.character(m$month)
m$nodes = as.numeric(as.character(m$nodes))
m$edges = as.numeric(as.character(m$edges))
m$mean.degree = as.numeric(as.character(m$mean.degree))
m$sd.degree = as.numeric(as.character(m$sd.degree))
m$mean.xvisiting = as.numeric(as.character(m$mean.xvisiting))
m$sd.xvisiting = as.numeric(as.character(m$sd.xvisiting))
m$centralization = as.numeric(as.character(m$centralization))
m$density = as.numeric(as.character(m$density))

library(ggplot2)
library(ggpubr)

#boxplots to visualize the distributions of the network parameters
if (visualize) {
  par(mfrow=c(2,4))
  boxplot(m$nodes)
  boxplot(m$edges)
  boxplot(m$mean.degree)
  boxplot(m$sd.degree)
  boxplot(m$mean.xvisiting)
  boxplot(m$sd.xvisiting)
  boxplot(m$centralization)
  boxplot(m$density)
  
  nodes_p = ggplot(data = m, aes(y=nodes)) + geom_boxplot()
  edges_p = ggplot(data = m, aes(y=edges)) + geom_boxplot()
  md_p = ggplot(data = m, aes(y=mean.degree)) + geom_boxplot()
  sd_p = ggplot(data = m, aes(y=sd.degree)) + geom_boxplot()
  mv_p = ggplot(data = m, aes(y=mean.xvisiting)) + geom_boxplot()
  sdv_p = ggplot(data = m, aes(y=sd.xvisiting)) + geom_boxplot()
  c_p = ggplot(data = m, aes(y=centralization)) + geom_boxplot()
  d_p = ggplot(data = m, aes(y=density)) + geom_boxplot()
  
  ggarrange(nodes_p, edges_p, md_p, sd_p, mv_p, sdv_p, c_p, d_p, 
            labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
            ncol = 4, nrow = 2)
}
  
#which nodes exists in all the networks?
commonNodes = V(graphs_list[[1]])$name
for(i in 2:length(graphs_list)) 
  commonNodes = intersect(commonNodes, V(graphs_list[[i]])$name)

#add Times of India to this list. NOTE: October 2014 does not have Times of India.
commonNodes = c(commonNodes, "The Times Of India Sites")

media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

commonNodes = intersect(commonNodes, media.breakdown[media.breakdown$Relevant == "Y",]$Media)

# groups to remove
# these groups are removed because the individual outlets are already there in commonNodes
# the only groups we keep are for those individual outlets that do not exist in all months (ie. in commonNodes)
# we remove QZ.COM because we have Atlantic Media but not Atlantic.
# So keeping QZ and removing Atlantic Media will remove Atlantic from our data
to_remove = c("India Today Group", "QZ.COM", "The Indian Express Group")

commonNodes = commonNodes[which(!commonNodes %in% to_remove)]

# for getting counts for tables
library(tidyverse)
media_breakdown = as_tibble(media.breakdown)

media_breakdown %>%
  filter(Media %in% commonNodes) %>%
  pull(Indian) %>%
  table()

media_breakdown %>%
  filter(Media %in% commonNodes) %>%
  pull(Digital) %>%
  table()

media_breakdown %>%
  filter(Media %in% commonNodes) %>%
  pull(Regional) %>%
  table()

#extract induced subgraph with only the common nodes that are relevant
#iterate over the graphs_list, get the induced subgraph and store it in a new list
#red_graphs_list (reduced graphs list)
red_graphs_list = vector("list", length(graphs_list))

for(i in 1:length(graphs_list)) {
  common.node.ids = unlist(lapply(commonNodes, FUN = function(x) which(x == V(graphs_list[[i]])$name)))
  red_graphs_list[[i]] = induced.subgraph(graph = graphs_list[[i]], vids = common.node.ids)
}

#do descriptive stats for reduced networks
#create an empty matrix of 9 columns.
red.m = matrix(NA,length(red_graphs_list),9)
#detach("package:sna", unload=TRUE)

#get network parameters
for(i in 1:length(red_graphs_list)){
  red.m[i,1] = months[i]
  red.m[i,2] = vcount(red_graphs_list[[i]])
  red.m[i,3] = ecount(red_graphs_list[[i]])
  red.m[i,4] = mean(degree(red_graphs_list[[i]],mode="all"))
  red.m[i,5] = sd(degree(red_graphs_list[[i]],mode="all"))
  red.m[i,6] = mean(E(red_graphs_list[[i]])$shared_audience)
  red.m[i,7] = sd(E(red_graphs_list[[i]])$shared_audience)
  red.m[i,8] = centr_degree(red_graphs_list[[i]],mode="all")$centralization
  red.m[i,9] = edge_density(red_graphs_list[[i]])
}

colnames(red.m) = c("month", "nodes", "edges","mean.degree", "sd.degree", 
                    "mean.xvisiting", "sd.xvisiting", "centralization", "density")

red.m = as.data.frame(red.m)

#change data types

red.m$month = as.character(red.m$month)
red.m$nodes = as.numeric(as.character(red.m$nodes))
red.m$edges = as.numeric(as.character(red.m$edges))
red.m$mean.degree = as.numeric(as.character(red.m$mean.degree))
red.m$sd.degree = as.numeric(as.character(red.m$sd.degree))
red.m$mean.xvisiting = as.numeric(as.character(red.m$mean.xvisiting))
red.m$sd.xvisiting = as.numeric(as.character(red.m$sd.xvisiting))
red.m$centralization = as.numeric(as.character(red.m$centralization))
red.m$density = as.numeric(as.character(red.m$density))


rnodes_p = ggplot(data = red.m, aes(y=nodes)) + geom_boxplot()
redges_p = ggplot(data = red.m, aes(y=edges)) + geom_boxplot()
rmd_p = ggplot(data = red.m, aes(y=mean.degree)) + geom_boxplot()
rsd_p = ggplot(data = red.m, aes(y=sd.degree)) + geom_boxplot()
rmv_p = ggplot(data = red.m, aes(y=mean.xvisiting)) + geom_boxplot()
rsdv_p = ggplot(data = red.m, aes(y=sd.xvisiting)) + geom_boxplot()
rc_p = ggplot(data = red.m, aes(y=centralization)) + geom_boxplot()
rd_p = ggplot(data = red.m, aes(y=density)) + geom_boxplot()

ggarrange(rnodes_p, redges_p, rmd_p, rsd_p, rmv_p, rsdv_p, rc_p, rd_p, 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 4, nrow = 2)


write.csv(m, "03_Auxiliary/raw_network_stats.csv", row.names = F)
write.csv(red.m, "03_Auxiliary/induced_network_stats.csv", row.names = F)

commonNodes = as.data.frame(commonNodes)
names(commonNodes) = "Media"
write.csv(commonNodes, "03_Auxiliary/common_nodes.csv", row.names = F)

KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)
all.media = as.data.frame(unique(KM.master.df$Media))
names(all.media) = "Media"
write.csv(all.media, "03_Auxiliary/all_media.csv", row.names = F)

save(red_graphs_list, file = "04_RData/02_induced_networks.RData")
