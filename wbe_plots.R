library(ggplot2)
library(RColorBrewer)
source("~/mm/wbe_mean_uncertainity.R")

dt <- read.csv("MTL_N_consumption2.csv")

dt$CAN_UC <- as.numeric(lapply(dt$Cannabis, function(x) mean_uncertainity(concentration = x, flow = dt$Flowrate/1000, population = dt$P_BOD, length(x))))

dt$Coc_UC <- as.numeric(lapply(dt$Cocaine, function(x) mean_uncertainity(concentration = x, flow = dt$Flowrate/1000, population = dt$P_BOD, length(x))))

dt$Cocaine_UC <- dt$Cocaine * dt$Coc_UC/2

dt$Cannabis_UC <- dt$Cannabis * dt$CAN_UC/2

ggplot(dt, aes(Date)) +
        geom_point(aes(y=Cannabis, color = day), size = 4) +
        geom_ribbon(aes(ymin = Cannabis - Cannabis_UC, ymax = Cannabis + Cannabis_UC, group = week), alpha = 0.2) + 
        ylim(0, NA) +
        scale_color_brewer(palette = "Set1") +
        theme_bw()

ggplot(dt, aes(Date)) +
        geom_point(aes(y=Cocaine, color = day), size = 4) +
        geom_ribbon(aes(ymin = Cocaine - Cocaine_UC, ymax = Cocaine + Cocaine_UC, group = week), alpha = 0.2) + 
        ylim(0, NA) +
        scale_color_brewer(palette = "Set1") +
        theme_bw()
