library(ggplot2)
#library(ggpmisc)
library(RColorBrewer)


source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("atrazine_extra.csv")
d3 <- summarySEwithin(d1, measurevar = "Atrazine", 
                      withinvars = c("Time", "Sample"), idvar = "Sample")

d4 <- dplyr::filter(d3, Sample == "AT10")

ggplot(d3, aes(x = Time, y= Atrazine, color = Sample)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = Atrazine - sd, ymax = Atrazine + sd), width = 0.1)+
        scale_color_brewer(palette = "Set1") +
        theme_bw(base_size = 16)+
        xlab("Time (h)") + 
        ylab(expression(paste("Atrazine (",  mu ,"g/L)")))

ggplot(d4, aes(x = Time, y= Atrazine, color = Sample)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = Atrazine - sd, ymax = Atrazine + sd), width = 0.1)
        
dev.print(png, "atrazine_change.png", width = 5, height = 5, res = 600, units = "in")
