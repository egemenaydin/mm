library(ggplot2)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)

dt <- read.csv("MTL_consumption_V2.csv")

row.names(dt) <- paste(dt$Date, dt$Location, sep = "_")

dt_pca <- dplyr::select(dt, Cocaine:Codeine, Location)

wbe.pca <- prcomp(dt_pca[, -length(dt_pca)],  scale = TRUE)

fviz_pca_biplot(wbe.pca, label="var", habillage=dt_pca$Location,
             addEllipses=TRUE, ellipse.level=0.95, pointsize = 3,
             labelsize = 5, repel = T) + 
        scale_color_brewer(palette="Set1") +
        theme_bw(base_size = 18)

ggsave("MTL_PCA.png", height = 8, width = 8, dpi = 600, unit = "in")


fviz_pca_ind(wbe.pca, select.ind = list(contrib = 15), repel = T)

ggsave("MTL_PCA_contributers.png", height = 8, width = 8, dpi = 600, unit = "in")
