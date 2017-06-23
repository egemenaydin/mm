library(ggplot2)
library(plyr)
d1 <- read.csv("olcum_sonuc_forheatmap_ing.csv")
l <- unique(d1$Numune)

td2 <- transform(d1, Numune = ordered(Numune, l))


ggplot(td2, aes(x = Numune, y = compound)) + 
        geom_tile(aes(fill = log2(concentration))) + 
        geom_text(aes(label = round(td2$concentration, 0))) +
        scale_fill_gradient(low = "green", high = "red") +
        theme_minimal(base_size =14) +
        xlab("") +
        ylab("") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

dev.print(png, "heatmap_tuzla_ib_ing.png", height = 5, width = 10, res = 600, unit = "in")
