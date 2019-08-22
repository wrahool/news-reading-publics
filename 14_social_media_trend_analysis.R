rm(list = ls())

library(ggplot2)
library(stringr)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

plot.df.master = read.csv("03_Auxiliary/temporal_sm_region_trends.csv", as.is = T)
ordered.months = read.csv("03_Auxiliary/ordered_months.csv", as.is = T)

reg = "S"

reg.plot.df = plot.df.master[plot.df.master$region == reg,]

reg.plot.df = merge(ordered.months, reg.plot.df)
reg.plot.df = reg.plot.df[order(reg.plot.df$n),]

#boxplots
#reg.plot.df$n2 <- factor(reg.plot.df$n, levels = as.character(1:nrow(ordered.months)))

#ggplot(data = reg.plot.df, aes(x = n2, y=t)) + 
#  geom_boxplot() + 
#  facet_grid(reg.plot.df$sm_2 ~ .) +
#  ggtitle("Social Media Consumption in North India")

# avg.t.by.sm.month = aggregate(t ~ month + sm_2, data = reg.plot.df, mean, na.rm = T)
# sd.t.by.sm.month = aggregate(t ~ month + sm_2, data = reg.plot.df, sd, na.rm = T)
# 
# avg.t.by.sm.month = merge(ordered.months, avg.t.by.sm.month)
# avg.t.by.sm.month = avg.t.by.sm.month[order(avg.t.by.sm.month$n),]
# 
# names(avg.t.by.sm.month)[4] = "mean.t"
# 
# sd.t.by.sm.month = merge(ordered.months, sd.t.by.sm.month)
# sd.t.by.sm.month = sd.t.by.sm.month[order(sd.t.by.sm.month$n),]
# 
# names(sd.t.by.sm.month)[4] = "sd.t"

avg.sd.t.by.sm.month = do.call(data.frame, aggregate(t ~ month + social.media, data = reg.plot.df,
                                 FUN = function(x) c(mn = mean(x, na.rm = T), sd = sd(x, na.rm = T))))

#calculate shared.audience/KM ratio
KM.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)

reg.plot.df.km = merge(reg.plot.df, KM.df, by.x = c("month", "reg.media"), by.y = c("Month", "Media"))
reg.plot.df.km$reg.media.pc = 100 * reg.plot.df.km$shared.audience / reg.plot.df.km$UV

avg.sd.t.reg.pc = do.call(data.frame, aggregate(cbind(reg.media.pc, t) ~ month + social.media, data = reg.plot.df.km,
                                   FUN = function(x) c(mn = mean(x, na.rm = T), sd = sd(x, na.rm = T))))


#################################################
#this loop is incomplete
# for(m in unique(reg.plot.df$month)) {
#   for(sm in unique(reg.plot.df$social.media)) {
#     t.by.sm.month = reg.plot.df[reg.plot.df$month == m & reg.plot.df$social.media == sm,]
#     avg.t = mean(t.by.sm.month, na.rm = T)
#   }
# }


# avg.sd.t.by.sm.month = merge(ordered.months, avg.sd.t.by.sm.month)
# avg.sd.t.by.sm.month = avg.sd.t.by.sm.month[order(avg.sd.t.by.sm.month$n),]

avg.sd.t.reg.pc.month = merge(ordered.months, avg.sd.t.reg.pc)

nrow(avg.sd.t.reg.pc.month[avg.sd.t.reg.pc.month$social.media == "T",])
nrow(avg.sd.t.reg.pc.month[avg.sd.t.reg.pc.month$social.media == "F",])
nrow(avg.sd.t.reg.pc.month[avg.sd.t.reg.pc.month$social.media == "Y",])
nrow(avg.sd.t.reg.pc.month[avg.sd.t.reg.pc.month$social.media == "I",])

missing.twitter.df = ordered.months[1:8,c(2,1)]
missing.twitter.df$social.media = "T"
missing.twitter.df$reg.media.pc.mn = NA
missing.twitter.df$reg.media.pc.sd = NA
missing.twitter.df$t.mn = NA
missing.twitter.df$t.sd = NA

avg.sd.t.reg.pc.month = rbind(avg.sd.t.reg.pc.month, missing.twitter.df)
avg.sd.t.reg.pc.month = avg.sd.t.reg.pc.month[order(avg.sd.t.reg.pc.month$n),]

avg.sd.t.reg.pc.month$year = str_sub(avg.sd.t.reg.pc.month$month, -4, -1)

#create a variable which makes every January => 1, February => 2, ..., December => 12
avg.sd.t.reg.pc.month$mon.in.yr = ifelse(avg.sd.t.reg.pc.month$year == "2014", avg.sd.t.reg.pc.month$n + 9,
                                     ifelse(avg.sd.t.reg.pc.month$year == "2015", avg.sd.t.reg.pc.month$n - 3,
                                            ifelse(avg.sd.t.reg.pc.month$year == "2016", avg.sd.t.reg.pc.month$n - 15,
                                                   ifelse(avg.sd.t.reg.pc.month$year == "2017", avg.sd.t.reg.pc.month$n - 27,
                                                          ifelse(avg.sd.t.reg.pc.month$year == "2018", avg.sd.t.reg.pc.month$n - 39, NA)))))

avg.sd.t.reg.pc.month$sm = ifelse(avg.sd.t.reg.pc.month$social.media == "F", "Facebook",
                              ifelse(avg.sd.t.reg.pc.month$social.media == "I", "Instagram",
                                     ifelse(avg.sd.t.reg.pc.month$social.media == "T", "Twitter",
                                            ifelse(avg.sd.t.reg.pc.month$social.media == "Y", "YouTube", NA))))

plot.title = paste("Social Media Consumption in", reg, "India", sep = " ")

# ggplot(data = avg.sd.t.by.sm.month, aes(x = mon.in.yr, y = t.mn)) +
#   geom_line() +
#   facet_wrap(avg.sd.t.by.sm.month$sm + avg.sd.t.by.sm.month$year ~ ., ncol = 5) +
#   scale_x_continuous("Month", breaks = seq(1, 12, 1)) +
#   ggtitle(plot.title)

write.csv(avg.sd.t.reg.pc.month, paste("03_Auxiliary/social_media_trends_", reg, ".csv", sep = ""), row.names = F)

############################
#all regions

social.media.trend.files = list.files("03_Auxiliary/")[grep("social_media", list.files("03_Auxiliary/"))]

social.media.trends.master = NULL
for(f in social.media.trend.files) {
  curr.social.media.trend.reg = read.csv(paste("03_Auxiliary/", f, sep = ""), as.is = T)
  curr.social.media.trend.reg$reg = str_sub(str_sub(f, -5), 1, 1)
  social.media.trends.master = rbind(social.media.trends.master, curr.social.media.trend.reg)
}

#this file name has social-media (not social_media) so that it cannot be grepped in the previous loop
write.csv(social.media.trends.master, "03_Auxiliary/social-media-trends-all-regions.csv", row.names = F)

#####
social.media.trends.master = read.csv("03_Auxiliary/social-media-trends-all-regions.csv", as.is = T)

#plots of mean t's
#plot trends by year
y = 2014
reqd.year.trends = social.media.trends.master[social.media.trends.master$year == y,]
ggplot(reqd.year.trends, aes(x = mon.in.yr, y = t.mn, color = reg)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(1:12,1)) +
  facet_grid(. ~ reqd.year.trends$sm) +
  ggtitle(paste("t of overlap of regional media with social media", y, sep = " "))

#plot trends by social media
s.m = "YouTube"

reqd.social.media.trends = social.media.trends.master[social.media.trends.master$sm == s.m,]
ggplot(reqd.social.media.trends, aes(x = mon.in.yr, y = t.mn, color = reg)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(1:12,1)) +
  facet_grid(. ~ reqd.social.media.trends$year) +
  ggtitle(paste("Yearly t of overlap of regional media with", s.m, sep = " "))

#by year and social media
reqd.social.media.trends.year = social.media.trends.master[social.media.trends.master$year == y & social.media.trends.master$sm == s.m,]
ggplot(reqd.social.media.trends.year, aes(x = mon.in.yr, y = t.mn, color = reg)) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = c(1:12,1)) +
  ggtitle(paste(y, s.m, sep = " "))

#plots of mean reg.pc's
#plot trends by year
y = 2014
reqd.year.trends = social.media.trends.master[social.media.trends.master$year == y,]
ggplot(reqd.year.trends, aes(x = mon.in.yr, y = reg.media.pc.mn, color = reg)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(1:12,1)) +
  facet_grid(. ~ reqd.year.trends$sm) +
  ggtitle(paste("Mean % overlap of regional media with social media", y, sep = " "))

#plot trends by social media
s.m = "YouTube"

reqd.social.media.trends = social.media.trends.master[social.media.trends.master$sm == s.m,]
ggplot(reqd.social.media.trends, aes(x = mon.in.yr, y = reg.media.pc.mn, color = reg)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(1:12,1)) +
  ylim(0,100) +
  facet_grid(. ~ reqd.social.media.trends$year) +
  ggtitle(paste("Yearly % overlap of regional media with", s.m, sep = " "))


