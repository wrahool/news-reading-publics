rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

library(igraph)
library(tidyverse)

############################################################################
# read keymeasures data 
# get only avg mins spent per visitor (ATV) for every month for every outlet

KM_files = list.files("01_Reports/CSV/KeyMeasures/")
KM_D_files = KM_files[sapply(KM_files, FUN = function(x) {grepl("D.csv", x)})]

oxford_KM_files = list.files("../../04_Oxford/Reports/CSV/KeyMeasures/")
oxford_KM_D_files = oxford_KM_files[sapply(oxford_KM_files, FUN = function(x) {grepl("D.csv", x)})]

KM.ATV.master.df = NULL
TI.ATV.df = NULL
for(f in KM_D_files) {
  
  #get the month year from the file name
  month.yr = paste(unlist(strsplit(f, "_"))[4:5], collapse = '')
  
  #read the file
  KM.df = read.csv(paste("01_Reports/CSV/KeyMeasures/", f, sep = ""), as.is = T)
  
  
  #1. get the total internet ATV per month
  #take the value from the first row, and 14th column
  KM.TI.ATV = KM.df[1,14]
  
  #add the monthyr and create a tuple
  curr.KM.TI.ATV = c(month.yr, KM.TI.ATV)
  
  #make a dataframe
  curr.KM.TI.ATV = as.data.frame(t(curr.KM.TI.ATV)) 
  
  #change column names
  names(curr.KM.TI.ATV) = c("Month", "TotalInternetATV")
  
  TI.ATV.df = rbind(TI.ATV.df, curr.KM.TI.ATV)
  
  #2. get all the keymeasures next
  #remove the first two rows. Keep only first and 14th columns
  KM.ATV.df = KM.df[-c(1,2),c(1,14)]
  
  #add a column with the monthyr
  KM.ATV.month.df = cbind(rep(month.yr, nrow(KM.ATV.df)), KM.ATV.df)
  
  #change column names
  names(KM.ATV.month.df) = c("Month", "Media", "ATV")
  
  KM.ATV.master.df = rbind(KM.ATV.master.df, KM.ATV.month.df)
}

#read oxford KM files

for(f in oxford_KM_D_files) {
  
  #get the month year from the file name
  month.yr = paste(unlist(strsplit(f, "_"))[4:5], collapse = '')
  
  #read the file
  KM.df = read.csv(paste("../../04_Oxford/Reports/CSV/KeyMeasures/", f, sep = ""), as.is = T)
  
  
  #1. get the total internet ATV per month
  #take the value from the first row, and 14th column
  KM.TI.ATV = KM.df[1,14]
  
  #add the monthyr and create a tuple
  curr.KM.TI.ATV = c(month.yr, KM.TI.ATV)
  
  #make a dataframe
  curr.KM.TI.ATV = as.data.frame(t(curr.KM.TI.ATV)) 
  
  #change column names
  names(curr.KM.TI.ATV) = c("Month", "TotalInternetATV")
  
  TI.ATV.df = rbind(TI.ATV.df, curr.KM.TI.ATV)
  
  #2. get all the keymeasures next
  #remove the first two rows. Keep only first and 14th columns
  KM.ATV.df = KM.df[-c(1,2),c(1,14)]
  
  #add a column with the monthyr
  KM.ATV.month.df = cbind(rep(month.yr, nrow(KM.ATV.df)), KM.ATV.df)
  
  #change column names
  names(KM.ATV.month.df) = c("Month", "Media", "ATV")
  
  KM.ATV.master.df = rbind(KM.ATV.master.df, KM.ATV.month.df)
}

write.csv(TI.ATV.df, "03_Auxiliary/Fall 19/total_internet_atv.csv", row.names = FALSE)
write.csv(KM.ATV.master.df, "03_Auxiliary/Fall 19/km_atv_master.csv", row.names = F)

