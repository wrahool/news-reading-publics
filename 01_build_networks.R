rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)

############################################################################
# read keymeasures

KM_files = list.files("01_Reports/CSV/KeyMeasures/")
KM_D_files = KM_files[sapply(KM_files, FUN = function(x) {grepl("D.csv", x)})]

oxford_KM_files = list.files("../../04_Oxford/Reports/CSV/KeyMeasures/")
oxford_KM_D_files = oxford_KM_files[sapply(oxford_KM_files, FUN = function(x) {grepl("D.csv", x)})]

KM.master.df = NULL
TI.df = NULL
for(f in KM_D_files) {
  
  #get the month year from the file name
  month.yr = paste(unlist(strsplit(f, "_"))[4:5], collapse = '')
  
  #read the file
  KM.df = read.csv(paste("01_Reports/CSV/KeyMeasures/", f, sep = ""), as.is = T)
  
  
  #1. get the total internet UVs per month
  #take the value from the first row, and second column
  KM.TI = KM.df[1,2]
  
  #add the monthyr and create a tuple
  curr.KM.TI = c(month.yr, KM.TI)
  
  #make a dataframe
  curr.KM.TI = as.data.frame(t(curr.KM.TI)) 
  
  #change column names
  names(curr.KM.TI) = c("Month", "TotalInternetUV")
  
  TI.df = rbind(TI.df, curr.KM.TI)
  
  #2. get all the keymeasures next
  #remove the first two rows. Keep only first three columns
  KM.df = KM.df[-c(1,2),1:3]
  
  #add a column with the monthyr
  KM.month.df = cbind(rep(month.yr, nrow(KM.df)), KM.df)
  
  #change column names
  names(KM.month.df) = c("Month", "Media", "UV", "PercentReach")
  
  KM.master.df = rbind(KM.master.df, KM.month.df)
}

#read oxford KM files

for(f in oxford_KM_D_files) {
  
  #get the month year from the file name
  month.yr = paste(unlist(strsplit(f, "_"))[4:5], collapse = '')
  
  #read the file
  KM.df = read.csv(paste("../../04_Oxford/Reports/CSV/KeyMeasures/", f, sep = ""), as.is = T)
  
  
  #1. get the total internet UVs per month
  #take the value from the first row, and second column
  KM.TI = KM.df[1,2]
  
  #add the monthyr and create a tuple
  curr.KM.TI = c(month.yr, KM.TI)
  
  #make a dataframe
  curr.KM.TI = as.data.frame(t(curr.KM.TI)) 
  
  #change column names
  names(curr.KM.TI) = c("Month", "TotalInternetUV")
  
  TI.df = rbind(TI.df, curr.KM.TI)
  
  #2. get all the keymeasures next
  #remove the first two rows. Keep only first three columns
  KM.df = KM.df[-c(1,2),1:3]
  
  #add a column with the monthyr
  KM.month.df = cbind(rep(month.yr, nrow(KM.df)), KM.df)
  
  #change column names
  names(KM.month.df) = c("Month", "Media", "UV", "PercentReach")
  
  KM.master.df = rbind(KM.master.df, KM.month.df)
}

############################################################################
# read edgelists

#demo = "Total Audience"
demo = ""
files = list.files(paste("02_Edgelists\\", demo, sep = ""))
files = files[sapply(files, FUN = function(x) {grepl(".csv", x)})] #only csv files

i = 1
graphs_list = vector("list", length(files))
months = NULL
for(f in files) {
  
  currMon = paste(unlist(strsplit(f, "_"))[3], unlist(strsplit(f, "_"))[4], sep = "")
  months = c(months, currMon)
  
  print(i)
  EL_df = read.csv(paste("02_Edgelists\\", demo, "\\", f, sep = ""), as.is = T)
  
  #reading only the first 3 columns. Ie, shared_audience.
  EL_df = EL_df[,1:3]
  
  #calculate phi
  #phi = (DN - AiAj)/(sqrt(AiAj(N-Ai)(N-Aj)))
  
  #add the month
  EL_df = cbind(rep(currMon, nrow(EL_df)), EL_df)
  names(EL_df)[1] = "month"
  
  #merge with TI.df to get the monthly TI
  EL_df = merge(EL_df, TI.df, by.x = "month", by.y = "Month")
  names(EL_df)[5] = "total_internet"
  
  #add Ai, Aj by merging twice with KM.master.df
  EL_df = merge(EL_df, KM.master.df, by.x = c("from", "month"), by.y = c("Media", "Month"))
  names(EL_df)[6:7] = c("from_UV", "from_pr")
  
  EL_df = merge(EL_df, KM.master.df, by.x = c("to", "month"), by.y = c("Media", "Month"))
  names(EL_df)[8:9] = c("to_UV", "to_pr")
  
  #calculate phi
  EL_df$to = as.character(EL_df$to)
  EL_df$from = as.character(EL_df$from)
  EL_df$month = as.character(EL_df$month)
  
  EL_df$shared_audience = as.numeric(as.character(EL_df$shared_audience))
  EL_df$total_internet = as.numeric(as.character(EL_df$total_internet))
  EL_df$from_UV = as.numeric(as.character(EL_df$from_UV))
  EL_df$from_pr = as.numeric(as.character(EL_df$from_pr))
  EL_df$to_UV = as.numeric(as.character(EL_df$to_UV))
  EL_df$to_pr = as.numeric(as.character(EL_df$to_pr))
  
  EL_df$phi_num = ((EL_df$shared_audience * EL_df$total_internet) - (EL_df$from_UV * EL_df$to_UV))
  EL_df$phi_denom = sqrt(EL_df$from_UV * EL_df$to_UV * (EL_df$total_internet - EL_df$from_UV) * (EL_df$total_internet - EL_df$to_UV))
  EL_df$phi = EL_df$phi_num / EL_df$phi_denom
  
  #calculate t
  #t = phi * sqrt(N-2) / sqrt(1 - phi^2)
  
  EL_df$t_num = EL_df$phi * sqrt((1000 * EL_df$total_internet) - 2)
  EL_df$t_denom = sqrt(1 - (EL_df$phi)^2)
  
  #the NaNs are for those rows where from to are equal.
  #check
  print("checking if NaNs are ok...")
  temp = EL_df[is.nan(EL_df$t_denom),c(1,3)]
  print(all(temp$to == temp$from)) #should be TRUE.
  #if not run this
  
  if(!all(temp$to == temp$from)) {
    for(ii in 1:nrow(temp)) {
      if(temp$to[ii] != temp$from[ii]) {
        print(ii)
        print(temp[ii,])
      }
    }
    rm(ii)
  }
  
  #these stupid rows are all edges between identical nodes but different names like NEWYORKER.COM and New Yorker
  
  rm(temp)
  
  EL_df[is.nan(EL_df$t_denom),]$t_denom = 0
  
  EL_df$t = EL_df$t_num / EL_df$t_denom
  
  final.EL_df = EL_df[,c(3,1,4,15)]
  g = graph.data.frame(final.EL_df, directed = F)
  
  g1 = simplify(g, remove.multiple = T, remove.loops = T, edge.attr.comb = "max")
  g1 = delete.edges(g1, which(E(g1)$shared_audience == 0))
  g1 = delete.vertices(g1, V(g1)[degree(g1) == 0])
  
  graphs_list[[i]] = g1
  i = i+1
}

write.csv(TI.df, "03_Auxiliary/total_internet_uv.csv", row.names = F)
write.csv(KM.master.df, "03_Auxiliary/km_master.csv", row.names = F)

months = as.data.frame(months)
write.csv(months, "03_Auxiliary/months.csv", row.names = F)

save(graphs_list, file = "04_RData/01_networks.RData")

#combine "E-PAPERVIEW.NET" and "E-PAPERVIEW.COM"
#g1 = merge.vertices(g, "E-PAPERVIEW.NET", "E-PAPERVIEW.COM", vertex.comb = "first", edge.comb = "max")