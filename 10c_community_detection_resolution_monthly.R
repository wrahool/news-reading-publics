setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

months = read.csv("03_Auxiliary/months.csv", as.is = T)
KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)

load("04_RData/03_filtered_networks.RData")

media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

# reqd.media = media.breakdown[(media.breakdown$News == "Y" &
#                                media.breakdown$Social == "N" & 
#                                media.breakdown$Fishy == "N" &
#                                media.breakdown$Group == "N") |
#                                media.breakdown$Media == "The Times Of India Sites" |
#                                media.breakdown$Media == "The Hindu Group",]$Media

reqd.media = media.breakdown[media.breakdown$Relevant == "Y",]$Media

for(i in 1:45) {
  print(i)
  g = filtered_graphs_list[[i]]
  g[from=V(g), to=V(g)] = 1
  sum(V(g)$name %in% reqd.media)
  
  for(v in V(g)$name) {
    
    E(g)[v %--% v]$shared_audience =
      KM.master.df[KM.master.df$Media == v & KM.master.df$Month == months$months[i],]$UV
  }
  
  WT_m = walktrap.community(g, weights = E(g)$shared_audience)
  print(length(WT_m))
  print("-------------")
}
