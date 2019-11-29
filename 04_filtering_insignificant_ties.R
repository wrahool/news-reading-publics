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

#compare network stats after thresholding with those of the original networks

library(ggplot2)
library(ggpubr)
library(igraph)

load("04_RData/01_networks.RData")
load("04_RData/02_induced_networks.RData")
load("04_RData/03_filtered_networks.RData")

months = read.csv("03_Auxiliary/months.csv", as.is = T)

months = months$months

compare.df = NULL

for(i in 1:45) {
  month = months[i]
  
  v_c1 = vcount(graphs_list[[i]])
  v_c2 = vcount(red_graphs_list[[i]])
  v_c3 = vcount(filtered_graphs_list[[i]])
  
  e_c1 = ecount(graphs_list[[i]])
  e_c2 = ecount(red_graphs_list[[i]])
  e_c3 = ecount(filtered_graphs_list[[i]])
  
  mean_d1 = mean(degree(graphs_list[[i]],mode="all"))
  mean_d2 = mean(degree(red_graphs_list[[i]],mode="all"))
  mean_d3 = mean(degree(filtered_graphs_list[[i]],mode="all"))
  
  sd1 = sd(degree(graphs_list[[i]],mode="all"))
  sd2 = sd(degree(red_graphs_list[[i]],mode="all"))
  sd3 = sd(degree(filtered_graphs_list[[i]],mode="all"))
  
  mean_sa1 = mean(E(graphs_list[[i]])$shared_audience)
  mean_sa2 = mean(E(red_graphs_list[[i]])$shared_audience)
  mean_sa3 = mean(E(filtered_graphs_list[[i]])$shared_audience)
  
  sd_sa1 = sd(E(graphs_list[[i]])$shared_audience)
  sd_sa2 = sd(E(red_graphs_list[[i]])$shared_audience)
  sd_sa3 = sd(E(filtered_graphs_list[[i]])$shared_audience)
  
  dc1 = centr_degree(graphs_list[[i]],mode="all")$centralization
  dc2 = centr_degree(red_graphs_list[[i]],mode="all")$centralization
  dc3 = centr_degree(filtered_graphs_list[[i]],mode="all")$centralization
  
  bc1 = centr_betw(graphs_list[[i]])$centralization
  bc2 = centr_betw(red_graphs_list[[i]])$centralization
  bc3 = centr_betw(filtered_graphs_list[[i]])$centralization
  
  cc1 = centr_clo(graphs_list[[i]])$centralization
  cc2 = centr_clo(red_graphs_list[[i]])$centralization
  cc3 = centr_clo(filtered_graphs_list[[i]])$centralization
  
  ec1 = centr_eigen(graphs_list[[i]])$centralization
  ec2 = centr_eigen(red_graphs_list[[i]])$centralization
  ec3 = centr_eigen(filtered_graphs_list[[i]])$centralization
  
  d1 = edge_density(graphs_list[[i]])
  d2 = edge_density(red_graphs_list[[i]])
  d3 = edge_density(filtered_graphs_list[[i]])
  
  month_row_raw = c(month, "raw", v_c1, e_c1, mean_d1, sd1, mean_sa1, sd_sa1,
                        dc1, bc1, cc1, ec1, d1)
  month_row_induced  = c(month, "induced", v_c2, e_c2, mean_d2, sd2, mean_sa2, sd_sa2,
                       dc2, bc2, cc2, ec2, d2)
  
  month_row_filtered  = c(month, "filtered", v_c3, e_c3, mean_d3, sd3, mean_sa3, sd_sa3,
                       dc3, bc3, cc3, ec3, d3)
  
  compare.df = rbind(compare.df, month_row_raw, month_row_induced, month_row_filtered)
}

compare.df = data.frame(compare.df, row.names = NULL)
names(compare.df) = c("month","before.after", "vertex_count", "edge_count",
                      "mean_degree", "stdev_degree", "mean_shad", "stdev_shad",
                      "DC", "BC", "CC", "EC", "density")


compare.df$month = as.character(compare.df$month)

compare.df$before.after = factor(compare.df$before.after,
                       levels = c('raw','induced', 'filtered'),ordered = TRUE)

compare.df$vertex_count = as.numeric(as.character(compare.df$vertex_count))
compare.df$edge_count = as.numeric(as.character(compare.df$edge_count))
compare.df$mean_degree = as.numeric(as.character(compare.df$mean_degree))
compare.df$stdev_degree = as.numeric(as.character(compare.df$stdev_degree))
compare.df$mean_shad = as.numeric(as.character(compare.df$mean_shad))
compare.df$stdev_shad = as.numeric(as.character(compare.df$stdev_shad))
compare.df$DC = as.numeric(as.character(compare.df$DC))
compare.df$BC = as.numeric(as.character(compare.df$BC))
compare.df$CC = as.numeric(as.character(compare.df$CC))
compare.df$EC = as.numeric(as.character(compare.df$EC))
compare.df$density = as.numeric(as.character(compare.df$density))


e_p = ggplot(compare.df, aes(x = before.after, y = edge_count, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "number of edges") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

md_p = ggplot(compare.df, aes(x = before.after, y = mean_degree, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "mean degree") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

sd_p = ggplot(compare.df, aes(x = before.after, y = stdev_degree, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "std. dev. of degree") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

msa_p = ggplot(compare.df, aes(x = before.after, y = mean_shad, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "mean shared audience") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

sdsa_p = ggplot(compare.df, aes(x = before.after, y = mean_shad, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "std. dev. of shared audience") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

dc_p = ggplot(compare.df, aes(x = before.after, y = DC, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "degree centralization") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

bc_p = ggplot(compare.df, aes(x = before.after, y = BC, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "betweenness centralization") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

cc_p = ggplot(compare.df, aes(x = before.after, y = CC, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "closeness centralization") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

ec_p = ggplot(compare.df, aes(x = before.after, y = EC, fill = before.after)) + 
  geom_boxplot() +
  labs(x = "", y = "eigenvector centralization") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_minimal() + theme(legend.position="none")

ggarrange(e_p, md_p, sd_p, msa_p, sdsa_p, dc_p, bc_p, cc_p, ec_p, 
          labels = toupper(letters)[1:9],
          ncol = 3, nrow = 3)

ggarrange(e_p, md_p, msa_p, dc_p, 
          labels = toupper(letters)[1:6],
          ncol = 2, nrow = 2)
