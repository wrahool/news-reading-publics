#setwd("C:\\Users\\Subhayan\\Documents\\Python Workspace\\India")
rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)

load("04_RData/02_induced_networks.RData")
load("04_RData/03_filtered_networks.RData")

#build one graph for 45 months
#convert each graph into an edgelist, then rbind all the edgelists into one large edgelist.
#Finally convert the edgelist into one graph and simplify the graph
#we don't need the t now, because we'll recalculate the t
#so we work with the red_graphs_list, not the filtered_graphs_list

media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

reqd.media = media.breakdown[(media.breakdown$News == "Y" &
                               media.breakdown$Social == "N" & 
                               media.breakdown$Fishy == "N" &
                               media.breakdown$Group == "N") |
                               media.breakdown$Media == "The Times Of India Sites",]$Media

red_graphs_list2 = vector("list", length(red_graphs_list))

for(i in 1:length(red_graphs_list2)) {
  reqd.node.ids = unlist(lapply(reqd.media, FUN = function(x) which(x == V(red_graphs_list[[i]])$name)))
  red_graphs_list2[[i]] = induced.subgraph(graph = red_graphs_list[[i]], vids = reqd.node.ids)
}

master.el = NULL
for(i in 1:length(red_graphs_list2)) {
  curr.el = as.data.frame(as_edgelist(red_graphs_list2[[i]]))
  names(curr.el) = c("from", "to")
  
  curr.el$shared_audience = E(red_graphs_list2[[i]])$shared_audience
  
  master.el = rbind(master.el, curr.el)
}

master.el$from = as.character(master.el$from)
master.el$to = as.character(master.el$to)

master.g = graph.data.frame(master.el, directed = F)

#now combine all edges between every pair of nodes, and sum them so that there's one "heavy" edge between two nodes
master.g = simplify(master.g, remove.loops = T, remove.multiple = T, edge.attr.comb = "sum")

#now convert that graph to an edgelist again so as to allow calculation of new t's.
#for this, we will need to sum keymeasures values as well. let's do that first.
KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)

KM.master.total.df = tapply(KM.master.df$UV, KM.master.df$Media, sum)
KM.master.total.df = as.data.frame(cbind(names(KM.master.total.df), KM.master.total.df), row.names = F)
names(KM.master.total.df) = c("media", "total.uv")

#now we need to get total internet UVs over 45 months.
TI.df = read.csv("03_Auxiliary/total_internet_uv.csv", as.is = T)

TTI = sum(TI.df$TotalInternetUV)

#now calculating the t values again, by first creating a graph edgelist
master.el = as.data.frame(as_edgelist(master.g))
names(master.el) = c("from", "to")

#append shared_audience column
master.el$shared_audience = E(master.g)$shared_audience

#merge with KM.master.total.df to get total.UV for "from" media
master.el = merge(master.el, KM.master.total.df, by.x = "from", by.y = "media")
names(master.el)[4] = "from_total_uv"

#merge with KM.master.total.df to get total.UV for "to" media
master.el = merge(master.el, KM.master.total.df, by.x = "to", by.y = "media")
names(master.el)[5] = "to_total_UV"

#add a column that contains TTI in every row
master.el$TTI = rep(TTI, nrow(master.el))

master.el = master.el[,c(2,1,3,4,5,6)]

master.el$from = as.character(master.el$from)
master.el$to = as.character(master.el$to)
master.el$shared_audience = as.numeric(as.character(master.el$shared_audience))
master.el$from_total_uv = as.numeric(as.character(master.el$from_total_uv))
master.el$to_total_UV = as.numeric(as.character(master.el$to_total_UV))
master.el$TTI = as.numeric(as.character(master.el$TTI))

#now calculate phi for each edge in this.

master.el$phi_num = ((master.el$shared_audience * master.el$TTI) - (master.el$from_total_uv * master.el$to_total_UV))
master.el$phi_denom = sqrt(master.el$from_total_uv * master.el$to_total_UV * (master.el$TTI - master.el$from_total_uv) * (master.el$TTI - master.el$to_total_UV))
master.el$phi = master.el$phi_num / master.el$phi_denom

#now calculate t for each edge

master.el$t_num = master.el$phi * sqrt((1000*master.el$TTI) - 2)
master.el$t_denom = sqrt(1- (master.el$phi)^2)
master.el$t = master.el$t_num / master.el$t_denom

final.master.el = master.el[,c(1,2,3,12)]

final.master.g = graph.data.frame(final.master.el, directed = F)

#there SHOULDN'T be multiple edges between two nodes in this edgelist because we created this edgelist
#from a simplified graph. CHECK to verify.
is_simple(final.master.g)

#Still just to be sure.
final.master.g = simplify(final.master.g, remove.multiple = T, remove.loops = T, edge.attr.comb = "max")

#again there SHOULDN'T be any edges with weight 0. SOMETHING'S WRONG IF THERE ARE/IS.
min(E(final.master.g)$shared_audience)
final.master.g = delete.edges(final.master.g, which(E(final.master.g)$shared_audience == 0))
final.master.g = delete.vertices(final.master.g, V(final.master.g)[degree(final.master.g) == 0])

filtered.master.g = delete.edges(final.master.g, which(abs(E(final.master.g)$t) <= 2.58))
filtered.master.g = delete.vertices(filtered.master.g, degree(filtered.master.g) == 0)

save(master.g, final.master.g, filtered.master.g, file = "04_RData/04_master_networks.RData")
