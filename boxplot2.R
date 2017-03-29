library(ggplot2)

d1 <- read.csv("lit_efficiency.csv")

ggplot(d1, aes(x = Compound, y = Efficiency)) +
        geom_boxplot() +
        geom_jitter(aes(color = Source), size = 3) +
        scale_color_manual(values = c("black", "red", "green")) +
        labs(x = "Compound", y = "Efficiency (%)") +
        theme_bw(base_size = 26) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
        

dev.print(png, "boxplot-lit-rev.png", height = 9, width = 16, res = 600, unit = "in")
