rm(list = ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

load("04_RData/05_edgelists.RData")

months = read.csv("03_Auxiliary/months.csv", as.is = T)
months = months$months

all.media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)
nat.media = all.media.breakdown[all.media.breakdown$State == "NT",]$Media

# boxplot_list1 = NULL
# boxplot_list2 = NULL

#el = el_list[[2]]
plot.df.master = NULL

for (j in 1:length(el_list)) {
#for (j in 1:5) {
  print(j)
  
  el = el_list[[j]]
  
  media.state = all.media.breakdown[,c("Media", "State")]
  
  #merge to get from.state and to.state
  el.state = merge(el, media.state, by.x = "from", by.y = "Media")
  names(el.state)[5] = "from.state"
  
  el.state = merge(el.state, media.state, by.x = "to", by.y = "Media")
  names(el.state)[6] = "to.state"
  
  el.nat.media.state = el.state[el.state$from.state == "NT" | 
                                            el.state$to.state == "NT",]
  
  el.nat.media.state = el.nat.media.state[el.nat.media.state$from.state != "None",]
  el.nat.media.state = el.nat.media.state[el.nat.media.state$to.state != "None",]
  
  #deduplicate the dataframe
  #to do this, need to eliminate those rows where the from and to = to and from
  #so we create a field, like this: if 'to' is social media, then the new field = to_from
  #else it is from_to
  #thus the new field looks like "THEHINDU.COM_XYZ.COM"
  el.nat.media.state$dedupcol = NULL
  el.nat.media.state$nm = NULL
  
  for(i in 1:nrow(el.nat.media.state)) {
    if(el.nat.media.state$to[i] %in% nat.media) {
      el.nat.media.state$dedupcol[i] = paste(el.nat.media.state$to[i], el.nat.media.state$from[i], sep = "_")
      el.nat.media.state$nm[i] = el.nat.media.state$to[i]
    }
    else {
      el.nat.media.state$dedupcol[i] = paste(el.nat.media.state$from[i], el.nat.media.state$to[i], sep = "_")
      el.nat.media.state$nm[i] = el.nat.media.state$from[i]
    }
  }
  
  el.nat.media.state = el.nat.media.state[!duplicated(el.nat.media.state$dedupcol),]
  
  el.nat.media.state$dedupcol = NULL
  
  plot.df = el.nat.media.state
  plot.df$state = NA
  
  for(i in 1:nrow(plot.df)) {
    if(plot.df$from.state[i] == "NT" & plot.df$to.state[i] == "NT") {
      plot.df$state[i] = "B"
    } else {
        if (plot.df$from.state[i] == "NT" & plot.df$to.state[i] != "NT") {
          plot.df$state[i] = plot.df$to.state[i]
        } else {
          if (plot.df$from.state[i] != "NT" & plot.df$to.state[i] == "NT") {
            plot.df$state[i] = plot.df$from.state[i]
          }
        }
    }
  }
  
  #ggplot(data = plot.df, aes(x=state, y=t)) + geom_boxplot()
  
  plot.df$region = NA
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
    else if (plot.df$state[i] == "B")
      plot.df$region[i] = "NT"
  }
  
  plot.df$month = months[j]
  
  plot.df$other.media = ifelse(plot.df$to %in% nat.media, plot.df$from, plot.df$to)
  
  plot.df = plot.df[,c("nm", "other.media", "shared_audience", "t", "state", "region", "month")]
  
  if("ABC Local" %in% plot.df$nm | "ABC Local" %in% plot.df$other.media) {
    print(" gotcha!")
  }
  
  #names(plot.df) = c("social.media", "reg.media", "shared.audience", "t", "state", "region", "month")
  
  plot.df.master = rbind(plot.df.master, plot.df)
  
  #boxplot_list1[[j]] = ggplot(data = plot.df, aes(x=region, y=t)) +
  #                    geom_boxplot() +
  #                    facet_grid(plot.df$sm_2 ~ .)
  
  #boxplot_list2[[j]] = ggplot(data = plot.df, aes(x=region, y=t)) +
  #                    geom_boxplot() +
  #                    facet_grid(. ~ plot.df$sm_2)
}

plot.df.master = plot.df.master[plot.df.master$nm != plot.df.master$other.media,]
write.csv(plot.df.master, "03_Auxiliary/temporal_nat_media_region_trends.csv", row.names = F)
