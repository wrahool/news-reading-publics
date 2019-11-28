setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")


library(igraph)
library(ggplot2)
library(gridExtra)

# read edgelists

#demo = "Total Audience"
demo = ""
files = list.files(paste("02_Edgelists\\", demo, sep = ""))
files = files[sapply(files, FUN = function(x) {grepl(".csv", x)})] #only csv files
TI.df = read.csv("03_Auxiliary/total_internet_uv.csv", as.is = T)
KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)

i = 1
el_list = vector("list", length(files))
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
  
  el_list[[i]] = final.EL_df
  i = i+1
}

save(el_list, file = "04_RData/05_edgelists.RData")
