library(ggplot2)
library(plyr)
d1 <- read.csv("tummaddeler_tuzla_kis.csv")
l <- unique(d1$Numune)

td2 <- transform(d1, Numune = ordered(Numune, l))


ggplot(td2, aes(x = Numune, y = variable)) + 
        geom_tile(aes(fill = log2(value))) + 
        geom_text(aes(label = round(td2$value, 0))) +
        scale_fill_gradient(low = "green", high = "red") +
        theme_minimal(base_size =14) +
        xlab("") +
        ylab("") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

dev.print(png, "heatmap_tuzla_kis_tr.png", height = 5, width = 10, res = 600, unit = "in")
