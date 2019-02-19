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
             labelsize = 5, repel = T, invisible = "quali",
             palette="Set1") + 
        #scale_color_brewer(palette="Dark2") +
        theme_minimal(base_size = 18) +
        labs(fill = "Location", color = "Location", shape = "Location")

ggsave("2city_PCA.png", height = 8, width = 8, dpi = 600, unit = "in")


fviz_pca_ind(wbe.pca, select.ind = list(contrib = 15), repel = T)

ggsave("2city_PCA_contributers.png", height = 8, width = 8, dpi = 600, unit = "in")


dt2 <- dplyr::filter(dt, Location == "MTL_South" | Location == "MTL_North")

dt_pca2 <- dplyr::select(dt2, Cocaine:Codeine, Location)

wbe.pca2 <- prcomp(dt_pca2[, -length(dt_pca)],  scale = TRUE)

fviz_pca_biplot(wbe.pca2, label="var", habillage=dt_pca2$Location,
                addEllipses=TRUE, ellipse.level=0.95, pointsize = 2,
                labelsize = 5, repel = T, invisible = "quali",
                palette="Set1") + 
        #scale_color_brewer(palette="Dark2") +
        theme_minimal(base_size = 18) +
        labs(fill = "Location", color = "Location", shape = "Location")

ggsave("MTL_PCA.png", height = 8, width = 8, dpi = 600, unit = "in")
