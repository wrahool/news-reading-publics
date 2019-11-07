rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

load("04_RData/05_edgelists.RData")

months = read.csv("03_Auxiliary/months.csv", as.is = T)
months = months$months

# boxplot_list1 = NULL
# boxplot_list2 = NULL

#el = el_list[[2]]
plot.df.master = NULL

for (j in 1:length(el_list)) {
  
  el = el_list[[j]]
  
  sm = c("Facebook", "Twitter", "YOUTUBE.COM", "INSTAGRAM.COM")
  el_sm = el[el$from %in% sm | el$to %in% sm,]
  
  all.media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)
  media.state = all.media.breakdown[,c(1,2)]
  
  el.sm.state = merge(el_sm, media.state, by.x = "from", by.y = "Media")
  names(el.sm.state)[5] = "from.state"
  
  el.sm.state = merge(el.sm.state, media.state, by.x = "to", by.y = "Media")
  names(el.sm.state)[6] = "to.state"
  
  
  #the next two lines only those edges that are between Social Media and regional outlets
  #choose only those rows, where the FROM MEDIA is social media or the FROM STATE isn't NT or None.
  el.sm.state = el.sm.state[(el.sm.state$from %in% sm) | !(el.sm.state$from.state %in% c("NT", "None")),]
  
  #choose only those rows, where the TO MEDIA is social media or the TO STATE isn't NT or None.
  el.sm.state = el.sm.state[(el.sm.state$to %in% sm) | !(el.sm.state$to.state %in% c("NT", "None")),]
  
  #now you remove those outletsedges that are between social media outlets 
  #choose only those rows, where the FROM STATE and To STATE are BOTH not None
  el.sm.state = el.sm.state[!((el.sm.state$from.state == "None") & (el.sm.state$to.state == "None")),]
  
  #this is same as doing
  #el.sm.state = el.sm.state[!((el.sm.state$from %in% sm) & (el.sm.state$to %in% sm)),]
  #check:
  #identical(el.sm.state[!((el.sm.state$from %in% sm) & (el.sm.state$to %in% sm)),],el.sm.state[!((el.sm.state$from.state == "None") & (el.sm.state$to.state == "None")),])
  
  #now we have only edges between a social media outlet and a regional outlet
  
  #deduplicate the dataframe
  #to do this, need to eliminate those rows where the from and to = to and from
  #so we create a field, like this: if 'to' is social media, then the new field = to_from
  #else it is from_to
  #thus the new field looks like "Facebook_XYZ.COM"
  el.sm.state$dedupcol = NULL
  el.sm.state$sm = NULL
  
  for(i in 1:nrow(el.sm.state)) {
    if(el.sm.state$to[i] %in% sm) {
      el.sm.state$dedupcol[i] = paste(el.sm.state$to[i], el.sm.state$from[i], sep = "_")
      el.sm.state$sm[i] = el.sm.state$to[i]
    }
    else {
      el.sm.state$dedupcol[i] = paste(el.sm.state$from[i], el.sm.state$to[i], sep = "_")
      el.sm.state$sm[i] = el.sm.state$from[i]
    }
  }
  
  el.sm.state$sm_2 = NULL
  
  for(i in 1:nrow(el.sm.state)) {
    if(el.sm.state$sm[i] == "Facebook")
      el.sm.state$sm_2[i] = "F"
    else if (el.sm.state$sm[i] == "INSTAGRAM.COM")
      el.sm.state$sm_2[i] = "I"
    else if (el.sm.state$sm[i] == "Twitter")
      el.sm.state$sm_2[i] = "T"
    else if (el.sm.state$sm[i] == "YOUTUBE.COM")
      el.sm.state$sm_2[i] = "Y"
  }
  
  
  el.sm.state.dedup = el.sm.state[!duplicated(el.sm.state$dedupcol),]
  
  plot.df = el.sm.state.dedup[,c(8,9,1,2,3,4,5,6)]
  
  plot.df$state = ifelse(plot.df$from.state == "None", plot.df$to.state, plot.df$from.state)
  plot.df = plot.df[,c(2,3,4,5,6,9)]
  
  #ggplot(data = plot.df, aes(x=state, y=t)) + geom_boxplot()
  
  plot.df$region = NULL
  for(i in 1:nrow(plot.df)) {
    if(plot.df$state[i] == "HD_RJ_PJ")
      plot.df$region[i] = "N"
    else if (plot.df$state[i] == "GJ")
      plot.df$region[i] = "W"
    else if (plot.df$state[i] == "AP_TG")
      plot.df$region[i] = "S"
    else if (plot.df$state[i] == "AS")
      plot.df$region[i] = "E"
    else if (plot.df$state[i] == "WB")
      plot.df$region[i] = "E"
    else if (plot.df$state[i] == "KA_MH")
      plot.df$region[i] = "W"
    else if (plot.df$state[i] == "JK")
      plot.df$region[i] = "N"
    else if (plot.df$state[i] == "TN")
      plot.df$region[i] = "S"
    else if (plot.df$state[i] == "KR")
      plot.df$region[i] = "S"
    else if (plot.df$state[i] == "KR_KA_TN")
      plot.df$region[i] = "S"
    else if (plot.df$state[i] == "OD")
      plot.df$region[i] = "E"
    else if (plot.df$state[i] == "KA")
      plot.df$region[i] = "S"
    else if (plot.df$state[i] == "MH")
      plot.df$region[i] = "W"
  }
  
  plot.df$month = months[j]
  
  plot.df$reg.media = ifelse(plot.df$to %in% sm, plot.df$from, plot.df$to)
  plot.df = plot.df[,c(1,9,4,5,6,7,8)]
  names(plot.df) = c("social.media", "reg.media", "shared.audience", "t", "state", "region", "month")
  
  plot.df.master = rbind(plot.df.master, plot.df)
  
  #boxplot_list1[[j]] = ggplot(data = plot.df, aes(x=region, y=t)) +
  #                    geom_boxplot() +
  #                    facet_grid(plot.df$sm_2 ~ .)
  
  #boxplot_list2[[j]] = ggplot(data = plot.df, aes(x=region, y=t)) +
  #                    geom_boxplot() +
  #                    facet_grid(. ~ plot.df$sm_2)
}

write.csv(plot.df.master, "03_Auxiliary/temporal_sm_region_trends.csv", row.names = F)
