library(ggplot2)
data <- read.csv("boxplot.csv")
means <- aggregate(Recovery ~ Matrix, data, mean)
ggplot(data, aes(x = Matrix, y = Recovery, fill = Matrix)) +
        geom_boxplot() +
        geom_point(size = 3, show_guide = FALSE) +
        stat_summary(fun.y = mean, colour = "purple", geom = "point", shape = 18, size = 4, show_guide = FALSE) +
        geom_text(data = means, aes(label = round(Recovery, 0), hjust = 2), size = 6) +
        scale_fill_manual(values = c("darkorange", "lightblue")) +
        labs(y = "Recovery (%)")
dev.print(pdf, "boxplot.pdf", height = 4.5, width = 8)
