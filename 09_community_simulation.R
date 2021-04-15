rm(list=ls())

setwd("C:\\Users\\Subhayan\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")


library(igraph)
library(ggplot2)
library(gridExtra)
library(tidyverse)

#WT1

load("04_RData/Fall 19/WT.Rdata")

common_nodes <- read_csv("03_Auxiliary/Fall 19/common_nodes.csv")
media_breakdown <- read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")

common_nodes_breakdown <- as.tbl(common_nodes %>% merge(media_breakdown))

community_tbl <- as_tibble(cbind(WT$names, WT$membership))

names(community_tbl) <- c("Media", "Community")

community_breakdown <- community_tbl %>%
  merge(common_nodes_breakdown) %>%
  as_tibble()

community_split <- community_breakdown %>%
                    group_by(Community)

community_split <- group_split(community_split)

community_languages <- lapply(community_split, FUN = function(x) {return(x$State)})

all_languages <- unique(common_nodes_breakdown$State)

all_occurrences_per_language <- NULL
for(lang in all_languages) {
  occurrences_of_lang <- sum(unlist(lapply(community_languages, FUN = function(x) {lang %in% x})))
  all_occurrences_per_language <- c(all_occurrences_per_language, occurrences_of_lang)
}
mean(all_occurrences_per_language)

obs_dispersion <- mean(all_occurrences_per_language)

#WT homogenieity

all_communities <- 1:max(WT$membership)

community_homogenieties <- NULL
for(community in all_communities) {
  community_homogeniety = mean(table(community_languages[[community]])/length(community_languages[[community]]))
  community_homogenieties = c(community_homogenieties, community_homogeniety)
}

obs_homo <- mean(community_homogenieties)

#0.7089947

#WT2

load("04_RData/Fall 19/WT2.Rdata")

common_nodes <- read_csv("03_Auxiliary/Fall 19/common_nodes.csv")
media_breakdown <- read_csv("03_Auxiliary/Fall 19/media_breakdown.csv")

common_nodes_breakdown <- as.tbl(common_nodes %>% merge(media_breakdown))

community_tbl <- as_tibble(cbind(WT2$names, WT2$membership))

names(community_tbl) <- c("Media", "Community")

community_breakdown <- community_tbl %>%
  merge(common_nodes_breakdown) %>%
  as_tibble()

community_split <- community_breakdown %>%
  group_by(Community)

community_split <- group_split(community_split)

community_languages <- lapply(community_split, FUN = function(x) {return(x$State)})

all_languages <- unique(common_nodes_breakdown$State)

all_occurrences_per_language <- NULL
for(lang in all_languages) {
  occurrences_of_lang <- sum(unlist(lapply(community_languages, FUN = function(x) {lang %in% x})))
  all_occurrences_per_language <- c(all_occurrences_per_language, occurrences_of_lang)
}
mean(all_occurrences_per_language)

obs_dispersion2 <- mean(all_occurrences_per_language)

#WT2 homogeneity
all_communities <- 1:max(WT2$membership)

community_homogenieties <- NULL
for(community in all_communities) {
  community_homogeniety = mean(table(community_languages[[community]])/length(community_languages[[community]]))
  community_homogenieties = c(community_homogenieties, community_homogeniety)
}

mean(community_homogenieties)
obs_homo2 <- mean(community_homogenieties)

#0.7698413

############################################################
#now simulations
#simulation1. unaugmented network.

set.seed(42)
load("04_RData/Fall 19/04_master_networks.RData")

dispersions <- NULL
sim_homogenieities <- NULL
sim_modularities <- NULL
for(i in 1:10000) {
  if(i %% 100 == 0) print(i)
  
  sim.g <- filtered.master.g
  
  #permute the other edge weights 
  E(sim.g)$shared_audience <- sample(E(sim.g)$shared_audience)
  
  sim.WT = cluster_walktrap(sim.g, weights = E(sim.g)$shared_audience)
  
  sim_community_tbl <- as_tibble(cbind(sim.WT$names, sim.WT$membership))
  
  names(sim_community_tbl) <- c("Media", "Community")
  
  sim_community_breakdown <- sim_community_tbl %>%
    merge(common_nodes_breakdown) %>%
    as_tibble()
  
  sim_community_split <- sim_community_breakdown %>%
    group_by(Community)
  
  sim_community_split <- group_split(sim_community_split)
  
  sim_community_languages <- lapply(sim_community_split, FUN = function(x) {return(x$State)})
  
  all_languages <- unique(common_nodes_breakdown$State)
  
  all_occurrences_per_language <- NULL
  for(lang in all_languages) {
    occurrences_of_lang <- sum(unlist(lapply(sim_community_languages, FUN = function(x) {lang %in% x})))
    all_occurrences_per_language <- c(all_occurrences_per_language, occurrences_of_lang)
  }
  
  #simulation dispersion
  dispersion = mean(all_occurrences_per_language)
  dispersions <- c(dispersion, dispersions)
  
  #simulation homogeniety
  community_tbl <- as_tibble(cbind(sim.WT$names, sim.WT$membership))
  
  names(community_tbl) <- c("Media", "Community")
  
  community_breakdown <- community_tbl %>%
    merge(common_nodes_breakdown) %>%
    as_tibble()
  
  community_split <- community_breakdown %>%
    group_by(Community)
  
  community_split <- group_split(community_split)
  
  community_languages <- lapply(community_split, FUN = function(x) {return(x$State)})
  
  all_communities <- 1:max(sim.WT$membership)
  
  community_homogenieties <- NULL
  for(community in all_communities) {
    community_homogeniety = mean(table(community_languages[[community]])/length(community_languages[[community]]))
    community_homogenieties = c(community_homogenieties, community_homogeniety)
  }
  
  sim_homogenieity <- mean(community_homogenieties)
  sim_homogenieities <- c(sim_homogenieities, sim_homogenieity)
}

sim_homogenieities_df <- data.frame(sim_homogenieities)

sim_plot1 <- ggplot(sim_homogenieities_df) + geom_density(aes(x=sim_homogenieities)) +
  geom_vline(xintercept = obs_homo, linetype="dashed", color = "red") +
  geom_vline(xintercept = quantile(sim_homogenieities, 0.95)) +
  xlim(c(0,1)) +
  theme_bw()

p_value1 <- sum(obs_homo < sim_homogenieities)/length(sim_homogenieities)
#0.0458

save(obs_homo, p_value1, sim_homogenieities, file = "04_RData/Fall 19/community_simulation1.Rdata")

#----------------------------------------------------
#simulation2: augmented network.
KM.master.df = read.csv("03_Auxiliary/km_master.csv", as.is = T)

KM.master.total.df = tapply(KM.master.df$UV, KM.master.df$Media, sum)
KM.master.total.df = as.data.frame(cbind(names(KM.master.total.df), KM.master.total.df), row.names = F)
names(KM.master.total.df) = c("media", "total.uv")

common.nodes.total.uv <- KM.master.total.df %>% filter(media %in% common_nodes$Media)
common.nodes.total.uv <- as.numeric(as.character(common.nodes.total.uv$total.uv))

dispersions <- NULL
sim_homogenieities2 <- NULL
for(i in 1:10000) {
  if(i %% 1 == 0) print(i)

  sim.g <- filtered.master.g
  
  sim.g[from=V(sim.g), to=V(sim.g)] = 1
  
  #permute the self-loop weights
  
  permute.common.nodes.total.UV  = sample(common.nodes.total.uv)
  
  j = 1
  for(v in V(sim.g)$name) {
    E(sim.g)[v %--% v]$shared_audience = permute.common.nodes.total.UV[j]
    j=j+1
  }
  
  #permute the other edge weights  
  E(sim.g)$shared_audience <- sample(E(sim.g)$shared_audience)
  
  sim.WT = cluster_walktrap(sim.g, weights = E(sim.g)$shared_audience)
  
  sim_community_tbl <- as_tibble(cbind(sim.WT$names, sim.WT$membership))
  
  names(sim_community_tbl) <- c("Media", "Community")
  
  sim_community_breakdown <- sim_community_tbl %>%
    merge(common_nodes_breakdown) %>%
    as_tibble()
  
  sim_community_split <- sim_community_breakdown %>%
    group_by(Community)
  
  sim_community_split <- group_split(sim_community_split)
  
  sim_community_languages <- lapply(sim_community_split, FUN = function(x) {return(x$State)})
  
  all_languages <- unique(common_nodes_breakdown$State)
  
  all_occurrences_per_language <- NULL
  for(lang in all_languages) {
    occurrences_of_lang <- sum(unlist(lapply(sim_community_languages, FUN = function(x) {lang %in% x})))
    all_occurrences_per_language <- c(all_occurrences_per_language, occurrences_of_lang)
  }
  
  #simulation dispersion
  dispersion = mean(all_occurrences_per_language)
  dispersions <- c(dispersion, dispersions)
  
  #simulation homogeniety
  community_tbl <- as_tibble(cbind(sim.WT$names, sim.WT$membership))
  
  names(community_tbl) <- c("Media", "Community")
  
  community_breakdown <- community_tbl %>%
    merge(common_nodes_breakdown) %>%
    as_tibble()
  
  community_split <- community_breakdown %>%
    group_by(Community)
  
  community_split <- group_split(community_split)
  
  community_languages <- lapply(community_split, FUN = function(x) {return(x$State)})
  
  all_communities <- 1:max(sim.WT$membership)
  
  community_homogenieties <- NULL
  for(community in all_communities) {
    community_homogeniety = mean(table(community_languages[[community]])/length(community_languages[[community]]))
    community_homogenieties = c(community_homogenieties, community_homogeniety)
  }
  
  sim_homogenieity <- mean(community_homogenieties)
  sim_homogenieities2 <- c(sim_homogenieities2, sim_homogenieity)
}

sim_homogenieities_df2 <- data.frame(sim_homogenieities2)

sim_plot2 <- ggplot(sim_homogenieities_df2) + geom_density(aes(x=sim_homogenieities2)) +
  geom_vline(xintercept = obs_homo2, linetype="dashed", color = "red") +
  geom_vline(xintercept = quantile(sim_homogenieities2, 0.95)) +
  xlim(c(0,1)) +
  theme_bw()

p_value2 <- sum(obs_homo2 < sim_homogenieities2)/length(sim_homogenieities2)

grid.arrange(sim_plot1, sim_plot2)

print(p_value2)

save(obs_homo2, p_value2, sim_homogenieities2, file = "04_RData/Fall 19/community_simulation2.Rdata")

# ggplot(sim_homogenieities_df) + 
#   geom_density(aes(x=sim_homogenieities)) +
#   geom_vline(xintercept = quantile(sim_homogenieities, 0.95)) +
#   geom_vline(xintercept = obs_homo, linetype = "dashed", color = "red") +
#   geom_vline(xintercept = obs_homo2, linetype = "dashed", color = "red") +
#   theme_bw()

# load("04_RData/Fall 19/community_simulation1.Rdata")
# load("04_RData/Fall 19/community_simulation2.Rdata")
# library(ggplot2)
# 
# sim_homogenieities_df <- data.frame(sim_homogenieities)
# 
# ggplot(sim_homogenieities_df) + 
#   geom_density(aes(x=sim_homogenieities)) +
#   geom_vline(xintercept = quantile(sim_homogenieities, 0.95)) +
#   geom_vline(xintercept = obs_homo, linetype = "dashed", color = "red") +
#   geom_vline(xintercept = obs_homo2, linetype = "dashed", color = "red") +
#   xlim(0, 1) +
#   theme_bw()

ggplot(sim_homogenieities_df) + 
  geom_density(aes(x=sim_homogenieities)) +
  geom_vline(xintercept = quantile(sim_homogenieities, 0.95)) +
  # geom_vline(xintercept = obs_homo, linetype = "dashed", color = "red") +
  geom_vline(xintercept = obs_homo, linetype = "dashed", color = "red") +
  xlim(0, 1) +
  theme_bw()
