library(ggplot2)
library(RColorBrewer)
source("~/mm/wbe_mean_uncertainity.R")

dt <- read.csv("MTL_S_consumption.csv")

dt$CAN_UC <- as.numeric(lapply(dt$Cannabis, function(x) mean_uncertainity(concentration = x, flow = dt$Flowrate/1000, population = dt$P_BOD, length(x))))

dt$Coc_UC <- as.numeric(lapply(dt$Cocaine, function(x) mean_uncertainity(concentration = x, flow = dt$Flowrate/1000, population = dt$P_BOD, length(x))))

dt$METH_UC_per <- as.numeric(lapply(dt$METH, function(x) mean_uncertainity(concentration = x, flow = dt$Flowrate/1000, population = dt$P_BOD, length(x))))

dt$MDMA_UC_per <- as.numeric(lapply(dt$MDMA, function(x) mean_uncertainity(concentration = x, flow = dt$Flowrate/1000, population = dt$P_BOD, length(x))))

dt$AMP_UC_per <- as.numeric(lapply(dt$AMP, function(x) mean_uncertainity(concentration = x, flow = dt$Flowrate/1000, population = dt$P_BOD, length(x))))

dt$Alcohol_UC_per <- as.numeric(lapply(dt$Alcohol, function(x) mean_uncertainity(concentration = x, flow = dt$Flowrate/1000, population = dt$P_BOD, length(x))))

dt$Cocaine_UC <- dt$Cocaine * dt$Coc_UC/2

dt$Cannabis_UC <- dt$Cannabis * dt$CAN_UC/2

dt$METH_UC <- dt$METH * dt$METH_UC_per/2

dt$MDMA_UC <- dt$MDMA * dt$MDMA_UC_per/2

dt$AMP_UC <- dt$AMP * dt$AMP_UC_per/2

dt$Alcohol_UC <- dt$Alcohol * dt$Alcohol_UC_per/2

options(scipen = 100000)

ggplot(dt, aes(Date)) +
        geom_point(aes(y=Cannabis, color = day), size = 4) +
        geom_ribbon(aes(ymin = Cannabis - Cannabis_UC, ymax = Cannabis + Cannabis_UC, group = week), alpha = 0.2) + 
        ylim(0, NA) +
        ylab("Cannabis consumption (mg/day/1000 inhabitants)") +
        scale_color_brewer(palette = "Set1") +
        scale_x_discrete(limits = dt$Date) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

ggsave("MTL_S_Cannabis.png", height = 5, width = 10, dpi = 600, unit = "in")

ggplot(dt, aes(Date, group = week)) +
        geom_point(aes(y=Cocaine, color = day), size = 4) +
        geom_ribbon(aes(ymin = Cocaine - Cocaine_UC, ymax = Cocaine + Cocaine_UC, group = week), alpha = 0.2) + 
        ylim(0, NA) +
        ylab("Cocaine consumption (mg/day/1000 inhabitants)") +
        scale_color_brewer(palette = "Set1") +
        scale_x_discrete(limits = dt$Date) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

ggsave("MTL_S_Cocaine.png", height = 5, width = 10, dpi = 600, unit = "in")

ggplot(dt, aes(Date, group = week)) +
        geom_point(aes(y=METH, color = day), size = 4) +
        geom_ribbon(aes(ymin = METH - METH_UC, ymax = METH + METH_UC, group = week), alpha = 0.2) + 
        ylim(0, NA) +
        scale_color_brewer(palette = "Set1") +
        scale_x_discrete(limits = dt$Date) +
        ylab("METH consumption (mg/day/1000 inhabitants)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

ggsave("MTL_S_METH.png", height = 5, width = 10, dpi = 600, unit = "in")

ggplot(dt, aes(Date, group = week)) +
        geom_point(aes(y=MDMA, color = day), size = 4) +
        geom_ribbon(aes(ymin = MDMA - MDMA_UC, ymax = MDMA + MDMA_UC, group = week), alpha = 0.2) + 
        ylim(0, NA) +
        scale_color_brewer(palette = "Set1") +
        scale_x_discrete(limits = dt$Date) +
        ylab("MDMA consumption (mg/day/1000 inhabitants)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

ggsave("MTL_S_MDMA.png", height = 5, width = 10, dpi = 600, unit = "in")

ggplot(dt, aes(Date, group = week)) +
        geom_point(aes(y=AMP, color = day), size = 4) +
        geom_ribbon(aes(ymin = AMP - AMP_UC, ymax = AMP + AMP_UC, group = week), alpha = 0.2) + 
        ylim(0, NA) +
        scale_color_brewer(palette = "Set1") +
        scale_x_discrete(limits = dt$Date) +
        ylab("AMP consumption (mg/day/1000 inhabitants)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

ggsave("MTL_S_AMP.png", height = 5, width = 10, dpi = 600, unit = "in")

ggplot(dt, aes(Date, group = week)) +
        geom_point(aes(y=Alcohol, color = day), size = 4) +
        geom_ribbon(aes(ymin = Alcohol - Alcohol_UC, ymax = Alcohol + Alcohol_UC, group = week), alpha = 0.2) + 
        ylim(0, NA) +
        scale_color_brewer(palette = "Set1") +
        scale_x_discrete(limits = dt$Date) +
        ylab("Alcohol consumption (L/day/1000 inhabitants)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

ggsave("MTL_S_Alcohol.png", height = 5, width = 10, dpi = 600, unit = "in")


