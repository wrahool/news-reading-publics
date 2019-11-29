rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(ggpubr)

load("04_RData/04_master_networks.RData")


# simulation
filtered.master.el = as.data.frame(as_edgelist(filtered.master.g))

##########################################
n = 1000
chunk = 100

taus = seq(0, 1, by = 0.001)

threshold.df = as.data.frame(matrix(data=NA, nrow = length(taus), ncol = chunk))
offset = 0
for(i in 1:n) {
  sim.el = filtered.master.el
  names(sim.el) = c("from", "to")
  sim.el$wt = sample(E(filtered.master.g)$shared_audience)
  
  sim.g = graph.data.frame(sim.el, directed = F)
  
  E(sim.g)$norm.wt = (E(sim.g)$wt - min(E(sim.g)$wt))/(max(E(sim.g)$wt) - min(E(sim.g)$wt))
  
  temp = sim.g
  rem.nodes = NULL
  for(threshold in taus) {
    temp = delete.edges(temp, which(E(temp)$norm.wt <= threshold))
    temp = delete.vertices(temp, degree(temp) == 0)
    rem.nodes = c(rem.nodes, vcount(temp))
  }
  
  threshold.df[,i-offset] = rem.nodes
  
  if(i %% (chunk/10) == 0) {
    print(i)
    print("-----------------")
  }
  
  if(i %% chunk == 0) {
    threshold.df = cbind(taus, threshold.df)
    names(threshold.df)[1] = "tau"
    write.csv(threshold.df, paste("03_Auxiliary/Simulation/thresholding_", i, ".csv", sep = ""), row.names = F)
    threshold.df = as.data.frame(matrix(data=NA, nrow = length(taus), ncol = chunk))
    offset = offset + chunk
  }
}

threshold.master.df = NULL
threshold.master.df$tau = taus
for(i in 1:(n/chunk)) {
  t.df = read.csv(paste("03_Auxiliary/Simulation/thresholding_", i*100, ".csv", sep = ""), as.is = T)
  names(t.df)[-1] = paste("V", (100*(i-1)+1):(i*100), sep = "")
  threshold.master.df = cbind(threshold.master.df, t.df[,-1])
}

threshold.master.df$avg = apply(threshold.master.df[,-1], 1, mean)
threshold.master.df$stdev = apply(threshold.master.df[-1], 1, sd)

threshold.final.df = threshold.master.df[,c("tau", "avg", "stdev")]

##########################################

# observed

observed.g = filtered.master.g
taus = seq(0, 1, by = 0.001)

E(observed.g)$norm.wt = (E(observed.g)$shared_audience - min(E(observed.g)$shared_audience))/
  (max(E(observed.g)$shared_audience) - min(E(observed.g)$shared_audience))

original_node_count = length(V(observed.g))

rem.nodes = NULL
n.components = NULL
connected = NULL
for(threshold in taus) {
  observed.g = delete.edges(observed.g, which(E(observed.g)$norm.wt <= threshold))
  observed.g = delete.vertices(observed.g, degree(observed.g) == 0)
  rem.nodes = c(rem.nodes, vcount(observed.g))
  n.components  = c(n.components, count_components(observed.g))
  connected = c(connected, is_connected(observed.g))
}

obs.threshold.df = as.data.frame(cbind(taus, rem.nodes, n.components, connected))
names(obs.threshold.df) = c("tau", "nodes", "components", "is_connected")

threshold.final.df$obs_rem_nodes = obs.threshold.df$nodes

rem_nodes_plot = ggplot(threshold.final.df, aes(x=tau)) + 
                    geom_line(aes(y = obs_rem_nodes*100/original_node_count)) +
                    geom_line(aes(y = avg*100/original_node_count), color = "blue") +
                    geom_line(aes(y = avg*100/original_node_count), linetype = "dashed", color = "blue") +
                    geom_line(aes(y = (avg + (3*stdev))*100/original_node_count), colour = "blue", linetype = "dashed") +
                    geom_line(aes(y = (avg - (3*stdev))*100/original_node_count), colour = "blue", linetype = "dashed") +
                    geom_hline(aes(yintercept = 8.6), linetype = "dashed", color = "red") +
                    geom_vline(aes(xintercept = 0.25), linetype = "dashed", color = "red")+
                    xlab("tau") +
                    theme_bw()+
                    theme(text = element_text(size=18))+
                    ylab("% remaining nodes")

obs_components_plot = ggplot(obs.threshold.df, aes(x=tau, y=components)) + 
  geom_line() +
  xlab("tau") +
  theme_bw()+
  theme(text = element_text(size=18))+
  ylab("number of connected components")

############################################

# get the core

thresholds = c(0.25)

KM_master = read_csv("03_Auxiliary/km_master.csv")
KM_master %>%
  group_by(Media) %>%
  summarise(meanPC = mean(PercentReach)) -> agg_PC
  

par(mfrow=c(1,1))
for(threshold in thresholds) {

  observed.g = filtered.master.g
  E(observed.g)$norm.wt = (E(observed.g)$shared_audience - min(E(observed.g)$shared_audience))/
    (max(E(observed.g)$shared_audience) - min(E(observed.g)$shared_audience))
  
  observed.g = delete.edges(observed.g, which(E(observed.g)$norm.wt <= threshold))
  observed.g = delete.vertices(observed.g, degree(observed.g) == 0)
  
  E(observed.g)$width <- E(observed.g)$shared_audience * 0.00005
  
  core_meanPCs <- agg_PC[agg_PC$Media %in% V(observed.g)$name,]
  
  for(i in 1:length(V(observed.g))) {
    print(V(observed.g)$name[i])
    print(which(core_meanPCs$Media == V(observed.g)$name[i]))
    print(agg_PC[agg_PC$Media == V(observed.g)$name[i],]$meanPC)
    V(observed.g)$size[i] = agg_PC[agg_PC$Media == V(observed.g)$name[i],]$meanPC
  }
  
  l <- layout_with_fr(observed.g)
  
  
  plot(observed.g, edge.color="orange",
       vertex.color="gray", vertex.label=NA,
       layout = l, main = threshold)
  
  plot(observed.g, edge.color="orange", vertex.color="gray",
      layout = l)
}

############################################

# remove an NRP and redo thresholding

load("04_RData/WT2.Rdata")

plot_list1 <- vector(mode = "list", length = length(WT2))
plot_list2 <- vector(mode = "list", length = length(WT2))

for(i in 1:length(WT2)) {

  to_remove = WT2[[i]]
  
  observed.g = filtered.master.g
  observed.g = delete.vertices(observed.g, which(V(observed.g)$name %in% to_remove))
  
  original_node_count = length(V(observed.g))
  
  # do thresholding
  
  taus = seq(0, 1, by = 0.001)
  
  E(observed.g)$norm.wt = (E(observed.g)$shared_audience - min(E(observed.g)$shared_audience))/
    (max(E(observed.g)$shared_audience) - min(E(observed.g)$shared_audience))
  
  rem.nodes = NULL
  n.components = NULL
  connected = NULL
  for(threshold in taus) {
    observed.g = delete.edges(observed.g, which(E(observed.g)$norm.wt <= threshold))
    observed.g = delete.vertices(observed.g, degree(observed.g) == 0)
    rem.nodes = c(rem.nodes, vcount(observed.g))
    n.components  = c(n.components, count_components(observed.g))
    connected = c(connected, is_connected(observed.g))
  }
  
  obs.threshold.df = as.data.frame(cbind(taus, rem.nodes, n.components, connected))
  names(obs.threshold.df) = c("tau", "nodes", "components", "is_connected")
  
  # ggplot(obs.threshold.df, aes(x=tau, y=nodes)) + 
  #   geom_line() +
  #   xlab("tau") +
  #   ylab("remaining nodes")
  
  plot_list1[[i]] = ggplot(obs.threshold.df, aes(x=tau, y=components)) + 
                      geom_line() +
                      ylim(0,5) +
                      xlab("tau") +
                      ylab("")
  
  plot_list2[[i]] = ggplot(obs.threshold.df, aes(x=tau, y=nodes*100/original_node_count)) + 
                      geom_line() +
                      geom_hline(aes(yintercept = 20), linetype = "dashed", color = "red") +
                      xlab("tau") +
                      ylab("% remaining nodes")
}

ggarrange(plotlist = plot_list1, ncol = 2, nrow = 7)
ggarrange(plotlist = plot_list2, ncol = 2, nrow = 7)



# explore the results

i = 4

to_remove = WT2[[i]]

observed.g = filtered.master.g
observed.g = delete.vertices(observed.g, which(V(observed.g)$name %in% to_remove))

# do thresholding

taus = seq(0, 1, by = 0.001)

E(observed.g)$norm.wt = (E(observed.g)$shared_audience - min(E(observed.g)$shared_audience))/
  (max(E(observed.g)$shared_audience) - min(E(observed.g)$shared_audience))

rem.nodes = NULL
n.components = NULL
connected = NULL
for(threshold in taus) {
  observed.g = delete.edges(observed.g, which(E(observed.g)$norm.wt <= threshold))
  observed.g = delete.vertices(observed.g, degree(observed.g) == 0)
  rem.nodes = c(rem.nodes, vcount(observed.g))
  n.components  = c(n.components, count_components(observed.g))
  connected = c(connected, is_connected(observed.g))
}

obs.threshold.df = as.data.frame(cbind(taus, rem.nodes, n.components, connected))
names(obs.threshold.df) = c("tau", "nodes", "components", "is_connected")

# ggplot(obs.threshold.df, aes(x=tau, y=nodes)) + 
#   geom_line() +
#   xlab("tau") +
#   ylab("remaining nodes")

components_plot_without_NRP = ggplot(obs.threshold.df, aes(x=tau, y=components)) + 
  geom_line(color = "blue", linetype = "longdash") +
  geom_line(aes(y=c(rep(1, 1000),0)), color = "red", linetype = "dashed") +
  xlab("tau") +
  ylab("number of connected components") +
  theme_bw()+
  theme(text = element_text(size=18))


components_plot_without_NRP 
  

thresholds = c(0, 0.009, 0.011, 0.026, 0.043, 0.052, 0.057, 0.147, 0.188, 0.261)

l = layout_with_fr(filtered.master.g)

dev.off()
par(mfrow=c(2,5))
for(threshold in thresholds) {
  observed.g = filtered.master.g
  observed.g = delete.vertices(observed.g, which(V(observed.g)$name %in% to_remove))
  E(observed.g)$norm.wt = (E(observed.g)$shared_audience - min(E(observed.g)$shared_audience))/
    (max(E(observed.g)$shared_audience) - min(E(observed.g)$shared_audience))
  
  observed.g = delete.edges(observed.g, which(E(observed.g)$norm.wt <= threshold))
  observed.g = delete.vertices(observed.g, degree(observed.g) == 0)
  
  plot(observed.g, vertex.label = NA, vertex.color = "skyblue1",
       main = paste0("tau = ", threshold))
}
