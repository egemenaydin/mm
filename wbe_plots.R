library(ggplot2)
library(RColorBrewer)
source("~/mm/wbe_mean_uncertainity.R")

dt <- read.csv("MTL_SHB_consumption_V1.csv")

dtS <- dplyr::filter(dt, Location == "SHB")

options(scipen = 100000)

dtS_tidy <- reshape2::melt(dtS, measure.vars = 4:10, value.name = "Consumption",
                           variable.name = "Drug")

p1 <- plyr::dlply(
        dtS_tidy, "Drug", function(x){
                x$UC_per <- as.numeric(lapply(x$Consumption, function(y) 
                        mean_uncertainity(concentration = y, flow = x$Flowrate/1000, population = x$P_BOD, length(y))))
                x$UC <- x$Consumption * x$UC_per/2
                ggplot(x, aes(Date)) +
                        geom_point(aes(y=Consumption, color = day), size = 4) +
                        geom_ribbon(aes(ymin = Consumption - UC,
                                        ymax = Consumption + UC, group = week),
                                    alpha = 0.3) +
                        ylim(0, NA) +
                        ylab("Consumption (mg/day/1000 inhabitants)") +
                        scale_color_brewer(palette = "Set1") +
                        scale_x_discrete(limits = dtS$Date) +
                        ggtitle(x$Drug) +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
        }
                
)

x <- length(p1)
tit <- unique(dtS_tidy$Drug)

for (i in 1:x){
        png(file = paste(tit[[i]], "_dotplot.png", sep = ""), height = 6, width = 10, res = 600, unit = "in")
        print(p1[[i]])
        dev.off()
}


p2 <- plyr::dlply(
        dtS_tidy, "Drug", function(x){
                x$UC_per <- as.numeric(lapply(x$Consumption, function(y) 
                        mean_uncertainity(concentration = y, flow = x$Flowrate/1000, population = x$P_BOD, length(y))))
                x$UC <- x$Consumption * x$UC_per/2
                ggplot(x, aes(Date)) +
                        geom_point(aes(y=Consumption, color = day), size = 4) +
                        geom_errorbar(position = position_dodge(width = 0.9), 
                                      aes(ymin = Consumption - UC,
                                          ymax = Consumption + UC), width = 0.3) +
                        ylim(0, NA) +
                        ylab("Consumption (mg/day/1000 inhabitants)") +
                        scale_color_brewer(palette = "Set1") +
                        scale_x_discrete(limits = dtS$Date) +
                        ggtitle(x$Drug) +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
        }
        
)

y <- length(p2)
tit2 <- unique(dtS_tidy$Drug)

for (i in 1:y){
        png(file = paste(tit2[[i]], "_dotplot_errorbar.png", sep = ""), height = 6, width = 10, res = 600, unit = "in")
        print(p2[[i]])
        dev.off()
}

