library(ggplot2)
library(dplyr)

d1 <- read.csv("leach2.csv")
d1 <- filter(d1, Compound == "PFOA" | Compound == "PFOS")

ggplot(d1, aes(x = Compound, y = Leachability)) +
        geom_boxplot(outlier.shape = NA) +
        stat_boxplot(geom = "errorbar", width = 0.2) +
        geom_jitter(aes(color = Soil), size = 3) +
        #scale_color_manual(values = c("black", "red", "green")) +
        labs(x = "", y = "Leachability (%)") +
        theme_bw(base_size = 32) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
        

dev.print(png, "leachability_legacy.png", height = 9, width = 12, res = 600, unit = "in")
