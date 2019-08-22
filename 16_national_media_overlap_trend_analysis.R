rm(list = ls())

library(ggplot2)
library(stringr)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

plot.df.master = read.csv("03_Auxiliary/temporal_nat_media_region_trends.csv", as.is = T)
ordered.months = read.csv("03_Auxiliary/ordered_months.csv", as.is = T)

KM.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)
plot.df.master = merge(plot.df.master, KM.df, by.x = c("other.media", "month"), by.y = c("Media", "Month"))

plot.df.master$aud.pc = plot.df.master$shared_audience/plot.df.master$UV

avg.sh.aud.by.reg.month = do.call(data.frame, aggregate(aud.pc ~ month + region, data = plot.df.master,
                                                        FUN = function(x) c(mn = mean(x, na.rm = T), 
                                                                            sd = sd(x, na.rm = T),
                                                                            N = length(x))))
avg.sh.aud.by.reg.month$aud.pc.se = avg.sh.aud.by.reg.month$aud.pc.sd/sqrt(avg.sh.aud.by.reg.month$aud.pc.N)

avg.sh.aud.by.reg.month$CI.upper = avg.sh.aud.by.reg.month$aud.pc.mn + 1.96 * avg.sh.aud.by.reg.month$aud.pc.se
avg.sh.aud.by.reg.month$CI.lower = avg.sh.aud.by.reg.month$aud.pc.mn - 1.96 * avg.sh.aud.by.reg.month$aud.pc.se

avg.sh.aud.by.reg.month = merge(ordered.months, avg.sh.aud.by.reg.month)
avg.sh.aud.by.reg.month = avg.sh.aud.by.reg.month[order(avg.sh.aud.by.reg.month$n),]

avg.sh.aud.by.reg.month$year = str_sub(avg.sh.aud.by.reg.month$month, -4, -1)

#create a variable which makes every January => 1, February => 2, ..., December => 12
avg.sh.aud.by.reg.month$mon.in.yr = ifelse(avg.sh.aud.by.reg.month$year == "2014", avg.sh.aud.by.reg.month$n + 9,
                                         ifelse(avg.sh.aud.by.reg.month$year == "2015", avg.sh.aud.by.reg.month$n - 3,
                                                ifelse(avg.sh.aud.by.reg.month$year == "2016", avg.sh.aud.by.reg.month$n - 15,
                                                       ifelse(avg.sh.aud.by.reg.month$year == "2017", avg.sh.aud.by.reg.month$n - 27,
                                                              ifelse(avg.sh.aud.by.reg.month$year == "2018", avg.sh.aud.by.reg.month$n - 39, NA)))))

ggplot(avg.sh.aud.by.reg.month, aes(x = mon.in.yr, y = aud.pc.mn)) +
  geom_line(size = 1, aes(color=region)) + 
  geom_ribbon(aes(ymin = avg.sh.aud.by.reg.month$CI.lower, ymax = avg.sh.aud.by.reg.month$CI.upper, fill = region), alpha = 0.4) +
  scale_x_continuous(breaks = c(1:12,1)) + 
  facet_grid(. ~ avg.sh.aud.by.reg.month$year) +
  ggtitle("Yearly % overlap of regional media with national media")

no.NT.df = avg.sh.aud.by.reg.month[avg.sh.aud.by.reg.month$region != "NT",]

ggplot(no.NT.df, aes(x = mon.in.yr, y = aud.pc.mn)) +
  geom_line(size = 1, aes(color=region)) + 
  geom_ribbon(aes(ymin = no.NT.df$CI.lower, ymax = no.NT.df$CI.upper, fill = region), alpha = 0.4) +
  scale_x_continuous(breaks = c(1:12,1)) + 
  facet_grid(. ~ no.NT.df$year) +
  scale_fill_manual(values=c("tan1", "red3", "royalblue3", "palegreen3")) +
  scale_color_manual(values=c("brown", "red", "blue", "green4")) +
  ggtitle("Yearly % overlap of regional media with national media")

#######################################################
##split into national legacy and national online media

media.breakdown = read.csv("03_Auxiliary/media_breakdown.csv", as.is = T)
media.breakdown.reqd = media.breakdown[,c("Media", "Digital")]

plot.df.master.2 = merge(plot.df.master, media.breakdown.reqd, by.x = "nm", by.y = "Media")

#remove the national media in other media
plot.df.master.2 = plot.df.master.2[plot.df.master.2$region != "NT",]

plot.df.master.2$digital.regional = paste(plot.df.master.2$Digital, plot.df.master.2$region, sep = '_')

avg.sh.aud.by.reg.month = do.call(data.frame, aggregate(aud.pc ~ month + digital.regional, data = plot.df.master.2,
                                                        FUN = function(x) c(mn = mean(x, na.rm = T), 
                                                                            sd = sd(x, na.rm = T),
                                                                            N = length(x))))
avg.sh.aud.by.reg.month$aud.pc.se = avg.sh.aud.by.reg.month$aud.pc.sd/sqrt(avg.sh.aud.by.reg.month$aud.pc.N)

avg.sh.aud.by.reg.month$CI.upper = avg.sh.aud.by.reg.month$aud.pc.mn + 1.96 * avg.sh.aud.by.reg.month$aud.pc.se
avg.sh.aud.by.reg.month$CI.lower = avg.sh.aud.by.reg.month$aud.pc.mn - 1.96 * avg.sh.aud.by.reg.month$aud.pc.se

avg.sh.aud.by.reg.month = merge(ordered.months, avg.sh.aud.by.reg.month)
avg.sh.aud.by.reg.month = avg.sh.aud.by.reg.month[order(avg.sh.aud.by.reg.month$n),]

avg.sh.aud.by.reg.month$year = str_sub(avg.sh.aud.by.reg.month$month, -4, -1)

#create a variable which makes every January => 1, February => 2, ..., December => 12
avg.sh.aud.by.reg.month$mon.in.yr = ifelse(avg.sh.aud.by.reg.month$year == "2014", avg.sh.aud.by.reg.month$n + 9,
                                           ifelse(avg.sh.aud.by.reg.month$year == "2015", avg.sh.aud.by.reg.month$n - 3,
                                                  ifelse(avg.sh.aud.by.reg.month$year == "2016", avg.sh.aud.by.reg.month$n - 15,
                                                         ifelse(avg.sh.aud.by.reg.month$year == "2017", avg.sh.aud.by.reg.month$n - 27,
                                                                ifelse(avg.sh.aud.by.reg.month$year == "2018", avg.sh.aud.by.reg.month$n - 39, NA)))))


ggplot(avg.sh.aud.by.reg.month, aes(x = mon.in.yr, y = aud.pc.mn)) +
  geom_line(size = 1, aes(color=digital.regional)) + 
  geom_ribbon(aes(ymin = avg.sh.aud.by.reg.month$CI.lower, ymax = avg.sh.aud.by.reg.month$CI.upper, fill = digital.regional), alpha = 0.4) +
  scale_x_continuous(breaks = c(1:12,1)) + 
  facet_grid(. ~ avg.sh.aud.by.reg.month$year) +
  ggtitle("Yearly % overlap of regional media with national media")
