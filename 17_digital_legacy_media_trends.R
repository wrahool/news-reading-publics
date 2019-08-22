rm(list = ls())
library(stringr)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

KM.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)

media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)

ordered.months = read.csv("03_Auxiliary/ordered_months.csv", as.is = T)

KM.type = merge(KM.df, media.breakdown, by = "Media")
KM.type.reqd = KM.type[KM.type$Indian == "Y",]

avg.pc.by.type.month = do.call(data.frame, aggregate(PercentReach ~ Month + Digital, data = KM.type.reqd,
                                                     FUN = function(x) c(mn = mean(x, na.rm = T), 
                                                                         sd = sd(x, na.rm = T),
                                                                         N = length(x))))

avg.pc.by.type.month$PercentReach.se = avg.pc.by.type.month$PercentReach.sd/sqrt(avg.pc.by.type.month$PercentReach.N)

avg.pc.by.type.month$CI.upper = avg.pc.by.type.month$PercentReach.mn + 1.96 * avg.pc.by.type.month$PercentReach.se
avg.pc.by.type.month$CI.lower = avg.pc.by.type.month$PercentReach.mn - 1.96 * avg.pc.by.type.month$PercentReach.se

avg.pc.by.type.month = merge(ordered.months, avg.pc.by.type.month, by.x = "month", by.y = "Month")
avg.pc.by.type.month = avg.pc.by.type.month[order(avg.pc.by.type.month$n),]

avg.pc.by.type.month$year = str_sub(avg.pc.by.type.month$month, -4, -1)

#create a variable which makes every January => 1, February => 2, ..., December => 12
avg.pc.by.type.month$mon.in.yr = ifelse(avg.pc.by.type.month$year == "2014", avg.pc.by.type.month$n + 9,
                                           ifelse(avg.pc.by.type.month$year == "2015", avg.pc.by.type.month$n - 3,
                                                  ifelse(avg.pc.by.type.month$year == "2016", avg.pc.by.type.month$n - 15,
                                                         ifelse(avg.pc.by.type.month$year == "2017", avg.pc.by.type.month$n - 27,
                                                                ifelse(avg.pc.by.type.month$year == "2018", avg.pc.by.type.month$n - 39, NA)))))

ggplot(avg.pc.by.type.month, aes(x = mon.in.yr, y = PercentReach.mn)) +
  geom_line(size = 1, aes(color=Digital)) + 
  geom_ribbon(aes(ymin = avg.pc.by.type.month$CI.lower, ymax = avg.pc.by.type.month$CI.upper, fill = Digital), alpha = 0.4) +
  scale_x_continuous(breaks = c(1:12,1)) + 
  facet_grid(. ~ avg.pc.by.type.month$year) +
  ggtitle("Yearly % reach of legacy and digital born National media")
