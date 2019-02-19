library(ggplot2)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)

dt <- read.csv("MTL_SHB_consumption_V1.csv")

row.names(dt) <- paste(dt$Date, dt$Location, sep = "_")

dt_pca <- dplyr::select(dt, Cocaine:Codeine, Location)

wbe.pca <- prcomp(dt_pca[, -length(dt_pca)],  scale = TRUE)

fviz_pca_biplot(wbe.pca, label="var", habillage=dt_pca$Location,
             addEllipses=TRUE, ellipse.level=0.95, pointsize = 2,
             labelsize = 5, repel = T, invisible = "quali") + 
        scale_color_brewer(palette="Dark2") +
        theme_minimal(base_size = 18)

ggsave("2city_PCA.png", height = 8, width = 8, dpi = 600, unit = "in")


fviz_pca_ind(wbe.pca, select.ind = list(contrib = 15), repel = T)

ggsave("2city_PCA_contributers.png", height = 8, width = 8, dpi = 600, unit = "in")
