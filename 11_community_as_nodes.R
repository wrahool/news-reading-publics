setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")


library(igraph)
library(ggplot2)
library(gridExtra)

load("04_RData/06_multilevel_comms.RData")
load("04_RData/04_master_networks.RData")

#read edgelists

EL.FG = read.csv("03_Auxiliary/FG_multilevel.csv", as.is = T)
names(EL.FG) = c("from", "to", "weight")

EL.L = read.csv("03_Auxiliary/L_multilevel.csv", as.is = T)
names(EL.L) = c("from", "to", "weight")

EL.WT = read.csv("03_Auxiliary/WT_multilevel.csv", as.is = T)
names(EL.WT) = c("form", "to", "weight")

media.details = read.csv("03_Auxiliary/avg_dc_pr_full.csv", as.is = T)
media.details = media.details[,c(1,3)]

get.edge.wt.communities(filtered.master.g, comm_list2_FG[[7]], comm_list2_FG[[4]])

EL.FG$wt.scale = (EL.FG$weight - min(EL.FG$weight))/(max(EL.FG$weight) - min(EL.FG$weight))

comm.g = graph.data.frame(EL.FG, directed = F)

v.num.outlets = sapply(as.numeric(V(comm.g)$name), FUN = function(x) { length(comm_list2_FG[[x]])})

#get the mean PR of the media outlets in each component
v.avg.pr = sapply(as.numeric(V(comm.g)$name), FUN = 
         function(x) { mean(media.details[media.details$Media %in% comm_list2_FG[[x]],]$avg.pr)})

v.median.pr = sapply(as.numeric(V(comm.g)$name), FUN = 
                    function(x) { median(media.details[media.details$Media %in% comm_list2_FG[[x]],]$avg.pr)})


v.df = as.data.frame(cbind(1:15, v.num.outlets, v.avg.pr, v.median.pr))
names(v.df)[1] = "name"

write.csv(v.df, "03_Auxiliary/v_df.csv", row.names = F)

plot(comm.g)

barplot(v.avg.pr, col = grey.colors(length(v.avg.pr)))

pal1 = colorspace::choose_palette()
pal2 = colorspace::choose_palette()

plot.igraph(comm.g,
            vertex.label=V(comm.g)$name,
            vertex.size = v.num.outlets, 
            vertex.color = pal(15),
            edge.color=pal2(105))

#plot.igraph(comm.g,vertex.label=V(comm.g)$name, vertex.size = v.num.outlets, 
#            vertex.color = v.avg.pr,
#            edge.color="blue",edge.width=log(log(E(comm.g)$weight)))

media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

comm_list2_FG[[1]]

table(sapply(comm_list2_FG[[1]], FUN = function(x) { return(media.breakdown[media.breakdown$Media == x,]$Regional)}))

#REGIONAL
get.regional.split = function(c) {
  NR = sum(media.breakdown[media.breakdown$Media %in% comm_list2_FG[[c]],]$Regional == "N", na.rm = T)
  R = sum(media.breakdown[media.breakdown$Media %in% comm_list2_FG[[c]],]$Regional == "Y", na.rm = T)
  NRp = 100 * NR / (NR + R)
  Rp = 100 * R / (NR + R)
  row1 = c(c, "R", R, Rp)
  row2 = c(c, "NR", NR, NRp)
  df = as.data.frame(rbind(row1, row2))
  names(df) = c("C", "type", "count", "perc")
  row.names(df) = NULL
  return(df)
}

regional.split = NULL
for(i in 1:length(comm_list2_FG)) {
  regional.split = rbind(regional.split, get.regional.split(i))
}

regional.split$count = as.numeric(as.character(regional.split$count))
regional.split$perc = as.numeric(as.character(regional.split$perc))

regional.split[14,]$count = 55

ggplot(data=regional.split, aes(x=C, y=perc, fill=type)) +
  geom_bar(stat="identity") + 
  #scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  scale_fill_manual(values = c("steelblue4", "palevioletred"))

#LANGUAGE
get.english.split = function(c) {
  NE = sum(media.breakdown[media.breakdown$Media %in% comm_list2_FG[[c]],]$English == "N", na.rm = T)
  E = sum(media.breakdown[media.breakdown$Media %in% comm_list2_FG[[c]],]$English == "Y", na.rm = T)
  NEp = 100 * NE / (NE + E)
  Ep = 100 * E / (NE + E)
  row1 = c(c, "E", E, Ep)
  row2 = c(c, "NE", NE, NEp)
  df = as.data.frame(rbind(row1, row2))
  names(df) = c("C", "type", "count", "perc")
  row.names(df) = NULL
  return(df)
}

english.split = NULL
for(i in 1:length(comm_list2_FG)) {
  english.split = rbind(english.split, get.english.split(i))
}

english.split$count = as.numeric(as.character(english.split$count))
english.split$perc = as.numeric(as.character(english.split$perc))

english.split[13,]$count = 62

ggplot(data=english.split, aes(x=C, y=perc, fill=type)) +
  geom_bar(stat="identity") + 
  #scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  scale_fill_brewer(palette="Set2")



