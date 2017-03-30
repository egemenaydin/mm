library(ggplot2)

d1 <- read.csv("lit_efficiency.csv")

ggplot(dplyr::filter(d1, Source == "BNR"), aes(x = Compound, y = Efficiency)) +
        geom_boxplot() +
        geom_jitter(aes(color = Source), size = 3) +
        geom_point(data = dplyr::filter(d1, Source != "BNR"), color = "Red", size = 5, shape = 17) +
        scale_color_manual(values = c("black", "red", "green")) +
        labs(x = "Compound", y = "Efficiency (%)") +
        theme_bw(base_size = 26) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 
        

dev.print(png, "boxplot-lit-rev-cl2.png", height = 9, width = 16, res = 600, unit = "in")
