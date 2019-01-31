library(ggplot2)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)

dt <- read.csv("MTL_consumption.csv")

dt_pca <- dplyr::select(dt, Cocaine:Amphetamine, Codeine, Location)

wbe.pca <- prcomp(dt_pca[, -7],  scale = TRUE)

fviz_pca_biplot(wbe.pca, label="var", habillage=dt_pca$Location,
             addEllipses=TRUE, ellipse.level=0.95, pointsize = 3,
             labelsize = 5) + 
        scale_color_brewer(palette="Set1") +
        theme_bw(base_size = 18)

ggsave("MTL_PCA.png", height = 8, width = 8, dpi = 600, unit = "in")
