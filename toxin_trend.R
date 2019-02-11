library(ggplot2)
#library(ggpmisc)
library(RColorBrewer)

d1 <- read.csv("LR.csv")
d2 <- dplyr::select(d1, Sample, Mean, Std..Deviation, Time, Concentration)

ggplot(d2, aes(x=Sample, y= Mean, color = factor(Time), shape = Concentration)) +
        geom_point() +
        geom_errorbar(aes(ymin = Std..Deviation, ymax = Std..Deviation), width = 0.1)+
        scale_color_brewer(palette = "Set1") +
        theme_bw(base_size = 16)+
        xlab("") + 
        ylab(expression(paste("Area")))
