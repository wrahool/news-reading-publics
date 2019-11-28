rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)
library(ggplot2)
library(gridExtra)
library(ggpubr)

load("04_RData/01_networks.RData")
load("04_RData/02_induced_networks.RData")

KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)
months = read.csv("03_Auxiliary/months.csv", as.is = T)
months = months$months

#turn this to T if you want to see the plots
visualize = T

#combining The Indian Express Group and INDIANEXPRESS.COM
#find vertex id of The Indian Express Group and INDIANEXPRESS.COM

for(i in 1:length(red_graphs_list)) {
  v1 = which("The Indian Express Group" == V(red_graphs_list[[1]])$name)
  v2 = which("INDIANEXPRESS.COM" == V(red_graphs_list[[1]])$name)
  
  #find total number of vertices in the original graph
  l = length(V(red_graphs_list[[1]])$name)
  
  mapping.vector = c(1:(v2-1), v1, v2:(l-1))
  
  test.g = contract(red_graphs_list[[1]], mapping.vector, vertex.attr.comb = "first")
  
  if(length(which("INDIANEXPRESS.COM" == V(test.g)$name)) > 0) {
    V(test.g)$name[which("INDIANEXPRESS.COM" == V(test.g)$name)] = "The Indian Express Group"
  }
  
  # test.g = simplify(test.g, remove.multiple = T, remove.loops = T, edge.attr.comb = "sum")
  test.g = simplify(test.g, remove.multiple = T, remove.loops = T, edge.attr.comb = "max")
}

####################################################################
#do centralization-reach analysis
deg.cent.df = vector("list", length(red_graphs_list))

for(i in 1:length(red_graphs_list)) 
  deg.cent.df[[i]] = centr_degree(red_graphs_list[[i]], normalized = T)


common.media = read.csv("03_Auxiliary/common_nodes.csv", as.is = T)

#create a dataframe with all possible "deg cent" and "perc reach" pairs.
dc_pr.df = NULL
for(m1 in common.media$Media) {
  for(m2 in months) {
    #find month index
    m2.index = which(m2 == months)
    
    #find vertex label index in the graph indexed by month index
    m1.index = which(m1 == V(red_graphs_list[[m2.index]])$name)
    
    #look up degree centrality
    dc = deg.cent.df[[m2.index]]$res[m1.index]
    
    #look up percentage reach
    pr = KM.master.df[KM.master.df$Month == m2 & KM.master.df$Media == m1,]$PercentReach
    
    #dc-pr tuple
    dc_pr = c(m1, m2, dc, pr)
    dc_pr.df = rbind(dc_pr.df, dc_pr)
  }
}

dc_pr.df = as.data.frame(dc_pr.df)
names(dc_pr.df) = c("Media", "Month", "DC", "PR")
row.names(dc_pr.df) = 1:nrow(dc_pr.df)

class(dc_pr.df$Media) #factor
class(dc_pr.df$Month) #factor
class(dc_pr.df$DC)    #factor
class(dc_pr.df$PR)    #factor

dc_pr.df$Media = as.character(dc_pr.df$Media)
dc_pr.df$Month = as.character(dc_pr.df$Month)

#if everything goes fine you should get only 2 warnins (1 for each of the following two lines)
#because of ToI in October2014
dc_pr.df$DC = as.numeric(as.character(dc_pr.df$DC))
dc_pr.df$PR = as.numeric(as.character(dc_pr.df$PR))

head(dc_pr.df[is.na(dc_pr.df$PR),])
head(dc_pr.df[is.na(dc_pr.df$DC),])

cor.test(dc_pr.df$DC, dc_pr.df$PR, method = "pearson")
cor.test(dc_pr.df$DC, dc_pr.df$PR, method = "spearman")
cor.test(dc_pr.df$DC, dc_pr.df$PR, method = "kendall")

ggplot(dc_pr.df, aes(x=log(DC), y=log(PR))) + 
  geom_point() + 
  labs(x = "log of degree centrality", y = "log of percentage reach")

#read in the media breakdown file
media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

#join the two tables
dc_pr.full.df = merge(dc_pr.df, media.breakdown, by.x = "Media", by.y = "Media")

dc_pr.full.df$DigitalSocial = paste(dc_pr.full.df$Digital, dc_pr.full.df$Social, sep = "_")

ggplot(dc_pr.df, aes(x=log(DC), y=log(PR))) + 
  geom_point() + 
  labs(x = "log of degree centrality", y = "log of percentage reach")


ggplot(dc_pr.full.df, aes(x=log(DC), y=log(PR), color=DigitalSocial)) + 
  geom_point() + 
  scale_color_brewer(palette="Spectral") +
  labs(x = "log of degree centrality", y = "log of percentage reach")

#################################################
#now calculate average DC and PR for each outlet and repeat

#average DC
avg.dc = tapply(dc_pr.full.df$DC, dc_pr.full.df$Media, FUN = function(x) {mean(x, na.rm = T)})
avg.dc.df = as.data.frame(cbind(names(avg.dc), avg.dc), row.names = F)
names(avg.dc.df) = c("Media", "avg.dc")

#average PR
avg.pr = tapply(dc_pr.full.df$PR, dc_pr.full.df$Media, FUN = function(x) {mean(x, na.rm = T)})
avg.pr.df = as.data.frame(cbind(names(avg.pr), avg.pr), row.names = F)
names(avg.pr.df) = c("Media", "avg.pr")

#merge
avg.dc.pr.df = merge(avg.dc.df, avg.pr.df, by.x = "Media", by.y = "Media")

#cast type
avg.dc.pr.df$Media = as.character(avg.dc.pr.df$Media)
avg.dc.pr.df$avg.dc = as.numeric(as.character(avg.dc.pr.df$avg.dc))
avg.dc.pr.df$avg.pr = as.numeric(as.character(avg.dc.pr.df$avg.pr))

cor.test(avg.dc.pr.df$avg.dc, avg.dc.pr.df$avg.pr, method = "pearson")
cor.test(avg.dc.pr.df$avg.dc, avg.dc.pr.df$avg.pr, method = "spearman")
cor.test(avg.dc.pr.df$avg.dc, avg.dc.pr.df$avg.pr, method = "kendall")

media.breakdown$X = NULL
avg.dc.pr.full.df = merge(avg.dc.pr.df, media.breakdown, by.x = "Media", by.y = "Media")
#avg.dc.pr.full.df$LegacySocial = paste(avg.dc.pr.full.df$Legacy, avg.dc.pr.full.df$Social, sep = "")



#These next few lines are no longer needed as we have earlier filtered out all non-relevant outlets
#only get outlets that are NEWS, NOT SOCIAL, NOT FISHY, and NOT CONGLOMERATES
#also mandatorily keep The Times Of India Sites
# reqd.media = avg.dc.pr.full.df[(avg.dc.pr.full.df$News == "Y" &
#                                  avg.dc.pr.full.df$Social == "N" & 
#                                  avg.dc.pr.full.df$Fishy == "N" &
#                                  avg.dc.pr.full.df$Group == "N") |
#                                  avg.dc.pr.full.df$Media == "The Times Of India Sites",]

#viz Indian vs Foreign

viz.df = avg.dc.pr.full.df[!is.na(avg.dc.pr.full.df$Indian),]

Indian_p = ggplot(viz.df, aes(x=log(avg.dc), y=log(avg.pr), color=Indian)) + 
  geom_point(size = 2, shape = 16) +
  scale_color_brewer(palette="Dark2") +
  labs(x = "log of degree centrality", y = "log of percentage reach")


#ggplot(avg.dc.pr.full.df, aes(x=log(avg.dc), y=log(avg.pr), color=Indian)) + 
#  geom_point(size = 2, shape = 6) +
#  scale_color_brewer(palette="Dark2") +
#  labs(x = "log of degree centrality", y = "log of percentage reach")

viz.df = avg.dc.pr.full.df[!is.na(avg.dc.pr.full.df$English),]

English_p = ggplot(viz.df, aes(x=log(avg.dc), y=log(avg.pr), color=English)) + 
  geom_point(size = 2, shape = 16) +
  scale_color_brewer(palette="Set1") +
  labs(x = "log of degree centrality", y = "log of percentage reach")

viz.df = avg.dc.pr.full.df[!is.na(avg.dc.pr.full.df$Regional),]

Regional_p = ggplot(viz.df, aes(x=log(avg.dc), y=log(avg.pr), color=Regional)) + 
  geom_point(size = 2) + 
  scale_color_brewer(palette="Set2") +
  labs(x = "log of degree centrality", y = "log of percentage reach")


viz.df = avg.dc.pr.full.df[!is.na(avg.dc.pr.full.df$Digital),]

Digital_p = ggplot(viz.df, aes(x=log(avg.dc), y=log(avg.pr), color=Digital)) + 
  geom_point(size = 2, shape = 16) +
  scale_color_brewer(palette="Set1") +
  labs(x = "log of degree centrality", y = "log of percentage reach")

ggarrange(Indian_p, Regional_p, English_p, Digital_p, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

#digital x Indian

viz.df = reqd.media[!is.na(reqd.media$Digital),]
viz.df = viz.df[!is.na(viz.df$Indian),]
viz.df = viz.df[!is.na(viz.df$English),]

viz.df$DI = paste(viz.df$Digital, viz.df$Indian)
viz.df$DIE = paste(viz.df$DI, viz.df$English)

ggplot(viz.df, aes(x=log(avg.dc), y=log(avg.pr), color=DI)) + 
  geom_point(size = 2, shape = 16) +
  scale_color_brewer(palette="Set1") +
  labs(x = "log of degree centrality", y = "log of percentage reach")

g1 = ggplot(viz.df, aes(x = DIE, y= log(avg.dc))) +
  geom_boxplot() +
  stat_summary(fun.y=median, geom="line", aes(group=1))  + 
  stat_summary(fun.y=median, geom="point")

g2 = ggplot(viz.df, aes(x = DIE, y= log(avg.pr))) +
  geom_boxplot() +
  stat_summary(fun.y=median, geom="line", aes(group=1))  + 
  stat_summary(fun.y=median, geom="point")

grid.arrange(g1, g2, nrow = 2)

write.csv(dc_pr.full.df, "03_Auxiliary/dc_pr_full.csv", row.names = F)
write.csv(avg.dc.pr.full.df, "03_Auxiliary/avg_dc_pr_full.csv", row.names = F)

######################

head(KM.master.df)
agg.KM.df = aggregate(KM.master.df$UV, by = list(KM.master.df$Media), FUN = mean)
names(agg.KM.df) = c("Media", "avg.UV")
agg.KM.df = agg.KM.df[order(-agg.KM.df$avg.UV),]
rownames(agg.KM.df) = NULL

