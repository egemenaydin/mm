library(FactoMineR)
library(factoextra)
#load data
df.m <- read.csv("base_ions_for_PCA.csv")

#read phenodata
PhD <- data.frame(read.csv("PhDP.csv"))
fNames <- as.vector(PhD$X)
fNames <- gsub("-", "\\.", fNames)
fSamples <- as.vector(PhD$class)
fSamples <- gsub("-", "\\.", fSamples)
grs <- unique(fSamples)

#tidy data
df <- df.m[match(fNames, df.m$X),]
df <- dplyr::filter(df, X != "Blank1")
df <- dplyr::filter(df, X != "blank8")
rownames(df) <- df$X
df$X <- fSamples[3:88]

#PCA
PCA <- PCA(df[,-1], scale.unit = TRUE, ncp = 5, graph = F)

#31 color palette
my.palette <- c("#000000","#831E3F", "#38D750", "#779BFB", "#7E6E0B","#FE39CD", "#79B609", "#17B9FB", "#C82519", "#122818", "#E9CBBD", "#FBB34B", "#671B62", "#00C684", "#711624", "#4A86FC", "#1E6015", "#F9366A", "#FF94B3", "#E8E207", "#2DE3F1", "#126C58", "#FADE8F", "#A9E9AA", "#A16C0B", "#241720", "#36A5B5", "#239D25", "#AC0337", "#F56828", "#501513")

#20 color palette
my.palette <- c("#000000","#831E3F", "#38D750", "#779BFB", "#7E6E0B", "#399EA4", "#FEFF89", "#ED7028", "#E2B5C9", "#3AEBB8", "#443B86", "#F85093", "#C6FC3C", "#2E5847", "#9A0D0F", "#F6535F", "#E0A7F5", "#603B0F", "#206CCE", "#5C6D05")

#26 color palette
my.palette <- c("#FE39CD", "#79B609", "#17B9FB", "#C82519", "#122818", "#E9CBBD", "#FBB34B", "#671B62", "#00C684", "#711624", "#4A86FC", "#1E6015", "#F9366A", "#FF94B3", "#E8E207", "#2DE3F1", "#126C58", "#FADE8F", "#A9E9AA", "#A16C0B", "#241720", "#36A5B5", "#239D25", "#AC0337", "#F56828", "#501513")

#26 color palette with 4 groups and 2 separate samples
my.palette <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026',
                '#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494',
                '#ffffcc','#d9f0a3','#addd8e','#78c679','#31a354','#006837',
                '#252525',
                '#edf8fb','#bfd3e6','#9ebcda','#8c96c6','#8856a7','#810f7c',
                '#54278f')

fviz_pca_ind(PCA,  geom = "point", habillage = as.factor(df$X), pointshape = 19, pointsize = 3) + 
        scale_color_manual(values = my.palette) +
        theme_bw(base_size = 20)+
        xlab("PC1 (28%)") + 
        ylab("PC2 (12%)") + 
        ggtitle("") +
        theme(legend.title = element_blank())


dev.print(png, file = "PCA_dots.png", height=6, width=10, res = 600, units = "in")


d1 <- read.csv("yuzeysel_su_CYA_normalize_kons-AO-RA.csv", header = TRUE, check.names = FALSE)
rownames(d1) <- d1$Area
PC <- prcomp(d1[, -c(1:4)], center = T, scale. = T)
pca_pr <- predict(PC)
ggplot2::ggplot(as.data.frame(pca_pr), aes(PC1, PC2, color = d1$Sp))+ 
        geom_point(size = 3) +
        scale_color_manual(values = my.palette) +
        theme_bw(base_size = 16)

PCA <- PCA(d1[, -c(1:4)], scale.unit = TRUE, ncp = 5, graph = F)
biplot(PC)
fviz_pca_ind(PC,  geom = "point", habillage = d1$Sp, pointshape = 19, pointsize = 2) + 
        scale_color_manual(values = my.palette)
fviz_pca_biplot(PCA, col.ind = my.palette, geom = "none")
fviz_pca_var(PC)

dev.print(png, file = "PCA_dots.png", height=9, width=16, res = 600, units = "in")
