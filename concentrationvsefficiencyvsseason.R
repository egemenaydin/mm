library(ggplot2)

d1 <- read.csv("efficiencyvsconc.csv")

options(scipen=100000000)

ggplot(d1, aes(x = Concentration, y= Compound, color = Efficiency, shape = Season)) +
        geom_point(size = 5) +
        ylab("")+
        xlab("Influent Concentration (ng/L)") +
        scale_color_gradient(low = "green", high = "red") +
        scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000)) +
        theme_bw(base_size =24)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

dev.print(png, "ces.png",height = 8, width = 12, res = 600, unit = "in")
