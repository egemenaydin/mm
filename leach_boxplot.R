library(ggplot2)

d1 <- read.csv("leach.csv")

ggplot(d1, aes(x = Soil, y = Leachability)) +
        geom_boxplot(outlier.shape = NA) +
        stat_boxplot(geom = "errorbar", width = 0.2) +
        geom_jitter(aes(color = PFAS), size = 3) +
        #scale_color_manual(values = c("black", "red", "green")) +
        labs(x = "Soil Type", y = "Leachability (%)") +
        theme_bw(base_size = 26) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
        

dev.print(png, "leachability.png", height = 9, width = 12, res = 600, unit = "in")
