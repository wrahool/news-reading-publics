setwd("C:\\Users\\Subhayan\\Documents\\Python Workspace\\India")

library(igraph)
library(ggplot2)
library(gridExtra)

load("RData/04_master_networks.RData")

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
    write.csv(threshold.df, paste("Auxiliary/Simulation/thresholding_", i, ".csv", sep = ""), row.names = F)
    threshold.df = as.data.frame(matrix(data=NA, nrow = length(taus), ncol = chunk))
    offset = offset + chunk
  }
}

threshold.master.df = NULL
threshold.master.df$tau = taus
for(i in 1:(n/chunk)) {
  t.df = read.csv(paste("Auxiliary/Simulation/thresholding_", i*100, ".csv", sep = ""), as.is = T)
  names(t.df)[-1] = paste("V", (100*(i-1)+1):(i*100), sep = "")
  threshold.master.df = cbind(threshold.master.df, t.df[,-1])
}

threshold.master.df$avg = apply(threshold.master.df[,-1], 1, mean)
threshold.master.df$stdev = apply(threshold.master.df[-1], 1, sd)

threshold.final.df = threshold.master.df[,c("tau", "avg", "stdev")]

##########################################

observed.g = filtered.master.g

E(observed.g)$norm.wt = (E(observed.g)$shared_audience - min(E(observed.g)$shared_audience))/
  (max(E(observed.g)$shared_audience) - min(E(observed.g)$shared_audience))

rem.nodes = NULL
for(threshold in taus) {
  observed.g = delete.edges(observed.g, which(E(observed.g)$norm.wt <= threshold))
  observed.g = delete.vertices(observed.g, degree(observed.g) == 0)
  rem.nodes = c(rem.nodes, vcount(observed.g))
}

obs.threshold.df = as.data.frame(cbind(taus, rem.nodes))
names(obs.threshold.df) = c("tau", "nodes")

threshold.final.df$obs = obs.threshold.df$nodes

ggplot(threshold.final.df, aes(tau)) + 
  geom_line(aes(y = avg + (6*stdev)), colour = "red", linetype = "dashed") +
  geom_line(aes(y = avg - (6*stdev)), colour = "red", linetype = "dashed") +
  geom_line(aes(y = avg), colour = "black") +
  geom_point(aes(y = obs), colour = "blue") +
  xlab("tau") +
  ylab("remaining nodes")
