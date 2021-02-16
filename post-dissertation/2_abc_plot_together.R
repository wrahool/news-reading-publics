library(ggplot2)
library(cowplot)

setwd("C:\\Users\\Subhayan Mukerjee\\Google Drive\\Annenberg UPenn\\0 Dissertation Project\\02_ComScoreData\\01_IndiaData\\")

load("04_RData/post-dissertation/digital_trends_ggplot.Rdata")
load("04_RData/post-dissertation/region_trends_ggplot.Rdata")
load("04_RData/post-dissertation/english_trends_ggplot.Rdata")

all_plots <- plot_grid(digital_median_plot, region_median_plot, english_median_plot, nrow = 3, labels = LETTERS[1:3])

ggsave(filename = "05_Plots/post-dissertation/longitudinal_trends.eps",
       plot = print(all_plots),
       device = cairo_eps)
