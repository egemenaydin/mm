library(ggplot2)

d1 <- read.csv("efficiencyvsconc_all.csv")

options(scipen=100000000)

ggplot(d1, aes(x = Compound, y= Efficiency)) +
        geom_boxplot(na.rm = T) +
        geom_point(aes(color = Season, shape = WWTP), size = 5) +
        ylab("")+
        xlab("Efficiency (%)") +
        scale_color_brewer(palette = "Set1") +
        #scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000)) +
        theme_bw(base_size =24)+
        theme(axis.text.x = element_text(angle = 75, hjust = 1))

dev.print(png, "ces.png",height = 8, width = 12, res = 600, unit = "in")
