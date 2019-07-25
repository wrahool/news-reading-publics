#setwd("C:\\Users\\Subhayan\\Documents\\Python Workspace\\India")
rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")


library(igraph)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(RColorBrewer)

load("04_RData/02_induced_networks.RData")
load("04_RData/03_filtered_networks.RData")
load("04_RData/01_networks.RData")

top.media.all.df = NULL
for(i in 1:length(red_graphs_list)) {
  top.media.df = as.data.frame(rev(tail(sort(eigen_centrality(red_graphs_list[[i]], 
                                   directed = FALSE, 
                                   scale = FALSE, 
                                   weights = E(red_graphs_list[[i]])$shared_audience)$vector), 10)))
  names(top.media.df) = c("eigenvector.centrality")
  
  top.media.df$media = names(rev(tail(sort(eigen_centrality(red_graphs_list[[i]], 
                                                            directed = FALSE, 
                                                            scale = FALSE, 
                                                            weights = E(red_graphs_list[[i]])$shared_audience)$vector), 10)))
  top.media.df$month = rep(i, 10)
  
  top.media.all.df = rbind(top.media.all.df, top.media.df)
}

row.names(top.media.all.df) = 1:nrow(top.media.all.df)

top.media.all.df = top.media.all.df[,c(3,2,1)]


ggplot(top.media.all.df, aes(x = month, y = eigenvector.centrality, colour = media)) + 
  geom_line() + 
  ylab(label="eigenvector centrality") + 
  xlab("month") +
  scale_colour_manual(values=c("#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
                               "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
                               "#5A0007", "#809693"))


