library(ggplot2)
data <- read.csv("AKO1-all.csv")
means <- aggregate(Abundance ~ Peak, data, mean)
options(scipen=1000000)
ggplot(data, aes(x = Peak, y = Abundance, fill = Peak)) +
        geom_boxplot() +
        geom_point(size = 4, show_guide = FALSE) +
        #stat_summary(fun.y = mean, colour = "purple", geom = "point", shape = 18, size = 4, show_guide = FALSE) +
        #geom_text(data = means, aes(label = round(Peak, 0), hjust = 2), size = 6) +
        scale_fill_manual(values = c("darkorange", "lightblue", "lightgreen", "maroon")) +
        labs(x = NULL, y = "Abundance (counts)")+
        theme_bw(base_size = 20)+
        theme(legend.position = "none")+
        facet_grid(. ~ Sulfate_red)
dev.print(pdf, "boxplot-all.pdf", height = 4.5, width = 8)
