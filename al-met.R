library(ggplot2)
library(ggpmisc)
library(RColorBrewer)

source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("norm_cya.csv")

d2 <- reshape2::melt(d1, id = c("Conc", "Time"))
d3 <- summarySEwithin(d2, measurevar = "value", 
                      withinvars = c("Conc", "variable", "Time"), idvar = "Conc")
d4 <- dplyr::filter(d3, variable == "LR" | variable == "HilR")
d5 <- dplyr::filter(d3, variable == "LR_Norm" | variable == "HilR_Norm")
d4 <- dplyr::filter(d3, variable == "LR" | variable == "HilR")

p1 <- plyr::dlply(d3, "variable", function(x){
        ggplot(x, aes(x = Time, y= value, color= Conc)) +
                geom_point(size = 4) +
                geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
                xlab("Time (hour)")+
                #ylab("Concentration (ug/L)") +
                theme_bw(base_size =18) +
                ggtitle(x$variable)
})

tit <- unique(d3$variable)



x <- length(p1)
for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""), height = 9, width = 16, res = 600, unit = "in")
        print(p1[[i]])
        dev.off()
}

ggplot(d4, aes(x = Time, y= value, color= variable), ) +
        geom_point(size = 4) +
        geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
        facet_grid(~Conc, labeller = label_both)+
        xlab("Time (hour)")+
        ylab("Concentration (ug/L)") +
        theme_bw(base_size =18)


ggplot(d5, aes(x = Time, y= value, color= variable), ) +
        geom_point(size = 4) +
        geom_errorbar(position = position_dodge(width = 0.3), 
                      aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
        facet_grid(~Conc, labeller = label_both)+
        xlab("Time (hour)")+
        ylab("Normalized Concentration") +
        theme_bw(base_size =18)
