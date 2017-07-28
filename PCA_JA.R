library(ggplot2)
df.m <- read.csv("PCA.csv")

g <- ggplot(df.m, aes(PC1, PC2))
g + geom_point(aes(shape = Inocculation, color = Environment), size = 5)+
        xlab("PC 1 (26.5%)") + ylab("PC 2 (23.8 %)") +
        scale_colour_brewer(palette = "Set1", labels = expression(KCl, Mg(NO[3])[2], MgSO[4], NaCl)) +
        theme_bw(base_size = 22)

dev.print(pdf, file = "PCA.pdf", height=7, width=11)
