#setwd("C:\\Users\\Subhayan\\Documents\\Python Workspace\\India")
rm(list=ls())

#scroll down for 42 months

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)
library(ggplot2)
library(gridExtra)

load("04_RData/04_master_networks.RData")

#centralization values for master graph
DC1.m = centr_degree(final.master.g)$centralization
BC1.m = centr_betw(final.master.g)$centralization
CC1.m = centr_clo(final.master.g)$centralization
EC1.m = centr_eigen(final.master.g)$centralization

#create filtered master graph, after removing egdes with t <= 2.58

filtered.mg = delete.edges(final.master.g, which(abs(E(final.master.g)$t) <= 2.58))
filtered.mg = delete.vertices(filtered.mg, degree(filtered.mg) == 0)

DC2.m = centr_degree(filtered.mg)$centralization
BC2.m = centr_betw(filtered.mg)$centralization
CC2.m = centr_clo(filtered.mg)$centralization
EC2.m = centr_eigen(filtered.mg)$centralization

central.df = read.csv("03_Auxiliary/centralization_before_after.csv", as.is = T)
  
central.df2 = as.data.frame(cbind(c(central.df$DC1, central.df$DC2),
                                  c(central.df$BC1, central.df$BC2),
                                  c(central.df$CC1, central.df$CC2),
                                  c(central.df$EC1, central.df$EC2),
                                  c(rep("Before", nrow(central.df)), rep("After", nrow(central.df)))))

names(central.df2) = c("deg.centr", "betw.centr", "clo.centr", "eigen.centr", "before.after")

central.df2$deg.centr = as.numeric(as.character(central.df2$deg.centr))
central.df2$betw.centr = as.numeric(as.character(central.df2$betw.centr))
central.df2$clo.centr = as.numeric(as.character(central.df2$clo.centr))
central.df2$eigen.centr = as.numeric(as.character(central.df2$eigen.centr))

central.df2$before.after <- factor(central.df2$before.after, levels = c("Before", "After"))

dc_p = ggplot(central.df2, aes(x = before.after, y = deg.centr)) + 
  geom_boxplot() +
  labs(x = "", y = "degree centrality")

bc_p = ggplot(central.df2, aes(x = before.after, y = betw.centr)) + 
  geom_boxplot() +
  labs(x = "", y = "betweenness centrality")

cc_p = ggplot(central.df2, aes(x = before.after, y = clo.centr)) + 
  geom_boxplot() +
  labs(x = "", y = "closeness centrality")

ec_p = ggplot(central.df2, aes(x = before.after, y = eigen.centr)) + 
  geom_boxplot() +
  labs(x = "", y = "eigenvector centrality")

grid.arrange(dc_p, bc_p, cc_p, ec_p, nrow = 1)

ggplot(central.df2, aes(x = before.after, y = deg.centr)) + 
  geom_boxplot() +
  labs(x = "", y = "degree centrality") #+
  #geom_point(DC1.m)

central.df$DC.change = (central.df$DC2 - central.df$DC1) * 100 / central.df$DC1
central.df$BC.change = (central.df$BC2 - central.df$BC1) * 100 / central.df$BC1
central.df$CC.change = (central.df$CC2 - central.df$CC1) * 100 / central.df$CC1
central.df$EC.change = (central.df$EC2 - central.df$EC1) * 100 / central.df$EC1

DC.m.change = (DC2.m - DC1.m) * 100 / DC1.m
BC.m.change = (BC2.m - BC1.m) * 100 / BC1.m
CC.m.change = (CC2.m - CC1.m) * 100 / CC1.m
EC.m.change = (EC2.m - EC1.m) * 100 / EC1.m

central.df$month = read.csv("03_Auxiliary/months.csv")$months

ordered.months = read.csv("03_Auxiliary/ordered_months.csv", as.is = T)

central.df.ordered = merge(ordered.months, central.df, by.x = "month", by.y = "month", all.x = T)

central.df.ordered = central.df.ordered[order(central.df.ordered$n),]

ggplot(central.df.ordered, aes(x=n, y=DC2 - mean(DC2, na.rm = T))) +
  geom_bar(stat="identity", width=0.5, color = "dodgerblue", fill = "dodgerblue") +
  xlab("month") +
  ylab("network centralization deviation from mean")

ggplot(central.df.ordered, aes(x=n, y=DC2)) +
  geom_line(color = "dodgerblue", size = 2) +
  geom_hline(yintercept = mean(central.df.ordered$DC2, na.rm = T)) +
  xlab("month") +
  ylab("network centralization") +
  scale_x_continuous(breaks=1:45)


ggplot() +
  geom_line(data = central.df.ordered, aes(x = n, y = DC1), color = "blue") +
  geom_line(data = central.df.ordered, aes(x = n, y = DC2), color = "red") +
  scale_x_continuous(breaks = 1:45) +
  geom_vline(xintercept = 26)
