#setwd("C:\\Users\\Subhayan\\Documents\\Python Workspace\\India")
rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)
library(ggplot2)
library(gridExtra)

load("04_RData/02_induced_networks.RData")

media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

reqd.media = media.breakdown[media.breakdown$Relevant == "Y",]$Media

red_graphs_list2 = vector("list", length(red_graphs_list))

for(i in 1:length(red_graphs_list2)) {
  reqd.node.ids = unlist(lapply(reqd.media, FUN = function(x) which(x == V(red_graphs_list[[i]])$name)))
  red_graphs_list2[[i]] = induced.subgraph(graph = red_graphs_list[[i]], vids = reqd.node.ids)
}


#edges that have unnaturally high t-values are between nodes that are either basically the same news outlet
#or one node is an outlet owned by another, so a visit to one is also recorded as a visit to the other.
#I have observed the following pairs of nodes to suffer from this problem. This is a non-exhaustive list:
#E-PAPERVIEW.NET -- E-PAPERVIEW.COM
#The Indian Express Group -- INDIANEXPRESS.COM
#QZ.COM -- Atlantic Media
#India Today Group -- India Today
#FIRSTPOST.COM -- Network 18

#first, let's remove the insignificant ties (that is those ties with t below the t = 2.58 threshold)
#and calculate the overall graph centralization before and after the removal of those ties

#this will contain the graphs with the insignificant ties removed.
filtered_graphs_list = vector("list", length(red_graphs_list))
central.df = NULL
for(i in 1:length(red_graphs_list2)){
  
  g = red_graphs_list2[[i]]
  DC1 = centr_degree(g)$centralization
  BC1 = centr_betw(g)$centralization
  CC1 = centr_clo(g)$centralization
  EC1 = centr_eigen(g)$centralization

  fg = delete.edges(g, which(abs(E(g)$t) <= 2.58))
  fg = delete.vertices(fg, degree(fg) == 0)
  DC2 = centr_degree(fg)$centralization
  BC2 = centr_betw(fg)$centralization
  CC2 = centr_clo(fg)$centralization
  EC2 = centr_eigen(fg)$centralization
  
  centr = c(DC1, DC2, BC1, BC2, CC1, CC2, EC1, EC2)
  
  if(DC1 > DC2 | BC1 > BC2 | CC1 > CC2 | DC1 > DC2) {
    print("WTF")
    print(i)
  }
    
  central.df = rbind(central.df, centr)
  filtered_graphs_list[[i]] = fg
}

central.df = as.data.frame(central.df, row.names = F)
names(central.df) = c("DC1", "DC2", "BC1", "BC2", "CC1", "CC2", "EC1", "EC2")

#Spring 2018 comment: note that for months 19, 22, and 33, closeness centrality DECREASES slightly after removing insignificant ties.
#Fall 2018 comment: Now, only in row 26, does CC decrease.
#Summer 2019 comment: CC never decreases.

#do Wilcoxon's Signed Rank test to see if DC2 is significantly different from DC1, etc.
wilcox.test(central.df$DC2, central.df$DC1, alternative = "two.sided", paired = T, conf.int = T, conf.level = 0.99)
wilcox.test(central.df$BC2, central.df$BC1, alternative = "two.sided", paired = T, conf.int = T, conf.level = 0.99)
wilcox.test(central.df$CC2, central.df$CC1, alternative = "two.sided", paired = T, conf.int = T, conf.level = 0.99)
wilcox.test(central.df$EC2, central.df$EC1, alternative = "two.sided", paired = T, conf.int = T, conf.level = 0.99)

#all the shifts in medians is indeed significant.

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

ggarrange(dc_p, bc_p, cc_p, ec_p, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

write.csv(central.df, "03_Auxiliary/centralization_before_after.csv", row.names = F)

save(filtered_graphs_list, file = "04_RData/03_filtered_networks.RData")
