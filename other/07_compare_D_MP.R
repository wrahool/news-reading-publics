# D vs MP analysis

rm(list=ls())
library(tidyverse)
library(ggpubr)

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

reqd_files = list.files("../01_IndiaData/../../04_Oxford/MP_D_comparison/CSV/Cleaned/")

# reqd_files = reqd_files[grep(pattern = "MP.csv", reqd_files)]


# Feb, Apr, May, Jul, Aug, Oct, Nov, Dec
#reqd_months = c("February2017", "April2017", "May2017", "July2017", "August2017", "October2017", "November2017", "December2017")
common_nodes = read_csv("03_Auxiliary/Fall 19/common_nodes.csv")

MP_df = NULL
for(f in reqd_files) {
  print(f)
  
  month = paste(unlist(strsplit(f, split = "_"))[4:5], collapse = "")
  month_MP_KM = read_csv(paste0("../01_IndiaData/../../04_Oxford/MP_D_comparison/CSV/Cleaned/", f))
  month_MP_KM %>%
    mutate(Month = month) %>%
    rename(Desktop_UV = Desktop, Mobile_UV = Mobile) %>%
    tail(n = -3) %>% # remove first 3 rows
    filter(Mobile_UV != "NaN") %>%
    filter(Desktop_UV != "NaN") %>%
    mutate(Desktop_UV = as.numeric(as.character(Desktop_UV)), Mobile_UV = as.numeric(as.character(Mobile_UV))) %>%
    filter(Media %in% common_nodes$Media) -> month_MP_KM
    
  MP_df %>% rbind(month_MP_KM) -> MP_df
}

media_breakdown = read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")

media_breakdown %>%
  filter(Media %in% common_nodes$Media) %>%
  filter(Indian == "Y") %>%
  select(Media, English) -> Indian_tbl

MP_df %>%
  inner_join(Indian_tbl) %>%
  mutate(English=replace(English, English=="Y", "English")) %>%
  mutate(English=replace(English, English=="B", "English")) %>%
  mutate(English=replace(English, English=="N", "Vernacular")) %>%
  gather(Platform, UV, -Media, -English, -Month) -> MP_df2

barplots <- vector(mode = "list", length = length(unique(MP_df2$Month)))
boxplots <- vector(mode = "list", length = length(unique(MP_df2$Month)))

for(i in 1:length(unique(MP_df2$Month))) {

  MP_df2 %>% 
    filter(Month==unique(MP_df2$Month)[i]) -> month_df
  
    boxplots[[i]]= ggplot(data = month_df, aes(x=English, y=UV)) +
      geom_boxplot(aes(fill=English)) +
      facet_wrap(.~gsub("_UV", "", Platform), scales = "free")+
      ylab(paste0(month_df$Month[1],  " UV"))+
      theme_bw()+
      theme(legend.position="none") 
  
    # barplots[[i]] = ggplot(data = month_df, aes(x=Media, y=UV)) +
    #     geom_bar(stat="identity", aes(fill=Platform)) +
    #     facet_wrap(~Platform + Regional) +
    #     theme_bw() +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
}

ggarrange(plotlist = boxplots, nrow = 4, ncol = 2)


wilcox.test(MP_df$Desktop_UV, MP_df$Mobile_UV)

for(month in unique(MP_df$Month)) {
  print(month)
  print(wilcox.test(MP_df[MP_df$Month == month,]$Desktop_UV,
                    MP_df[MP_df$Month == month,]$Mobile_UV))
  print("---------------")
}

ggplot(data = MP_df2, aes(x=English, y=UV)) +
  geom_boxplot(aes(fill=English)) +
  facet_wrap(.~gsub("_UV", "", Platform), scales = "free")+
  ylab(paste0(month_df$Month[1],  " UV"))+
  theme_bw()+
  theme(legend.position="none")

