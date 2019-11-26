setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)
library(ggplot2)
library(gridExtra)
library(tidyverse)

load("04_RData/04_master_networks.RData")


##########################################

observed.g = filtered.master.g
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


ggplot(obs.threshold.df, aes(x=tau, y=nodes)) + 
  geom_line() +
  xlab("tau") +
  ylab("remaining nodes")

ggplot(obs.threshold.df, aes(x=tau, y=components)) + 
  geom_line() +
  xlab("tau") +
  ylab("remaining nodes")

# explore the results
tau = 0.583

observed.g = filtered.master.g
E(observed.g)$norm.wt = (E(observed.g)$shared_audience - min(E(observed.g)$shared_audience))/
  (max(E(observed.g)$shared_audience) - min(E(observed.g)$shared_audience))

observed.g = delete.edges(observed.g, which(E(observed.g)$norm.wt <= tau))
observed.g = delete.vertices(observed.g, degree(observed.g) == 0)

############################################

# remove an NPR and redo thresholding

load("04_RData/WT2.Rdata")

to_remove = WT2[[2]]

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

ggplot(obs.threshold.df, aes(x=tau, y=components)) + 
  geom_line() +
  xlab("tau") +
  ylab("number of connected components")

# explore the results
tau = 0.069

observed.g = filtered.master.g
observed.g = delete.vertices(observed.g, which(V(observed.g)$name %in% to_remove))

E(observed.g)$norm.wt = (E(observed.g)$shared_audience - min(E(observed.g)$shared_audience))/
  (max(E(observed.g)$shared_audience) - min(E(observed.g)$shared_audience))

observed.g = delete.edges(observed.g, which(E(observed.g)$norm.wt <= tau))
observed.g = delete.vertices(observed.g, degree(observed.g) == 0)
plot(observed.g)
