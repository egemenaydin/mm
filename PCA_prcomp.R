library(ggplot2)
library(RColorBrewer)
data <- read.csv("sample2.csv", header = TRUE, check.names = FALSE)
data2 <- read.csv("sample2.csv", header = TRUE, check.names = FALSE)
rownames(data) <- make.names(data$Compound, unique = TRUE)
data$Compound <- NULL

data_polished <- data[, apply(data, 2, var, na.rm = TRUE) != 0]
data_polished[data_polished == 1] <- 0.5*(min(data_polished[data_polished>0],na.rm=TRUE))

log.data <- log2(data_polished[ , 2:length(data_polished)])

Samples <- data2$Compound
pca <- prcomp(log.data, center = TRUE, scale. = TRUE)

PC <- predict(pca)
write.csv(PC, file = "PCA.csv")
x <- read.csv("PCA.csv", row.names = 1)
summary <- summary(pca)
loadings <- pca$rotation

write.csv(loadings, file="loadings.csv")
y <- read.csv("loadings.csv", row.names = 1)

my.palette <- c("#000000","#831E3F", "#38D750", "#779BFB", "#7E6E0B", "#399EA4", "#FEFF89", "#ED7028", "#E2B5C9", "#3AEBB8", "#443B86", "#F85093", "#C6FC3C", "#2E5847", "#9A0D0F", "#F6535F", "#E0A7F5", "#603B0F", "#206CCE", "#5C6D05")

x_lab <- sprintf ('PC1 (%0.1f%%)', 100*pca$sdev[1]^2/sum(pca$sdev^2))
y_lab <- sprintf ('PC2 (%0.1f%%)', 100*pca$sdev[2]^2/sum(pca$sdev^2))

g <- ggplot(x, aes(PC1, PC2))
g + geom_point(aes(color = Samples), size = 5) + theme_bw(base_size = 12) + xlab(x_lab) + ylab(y_lab) + scale_colour_manual(values = my.palette)

dev.print(pdf, file = "PCA.pdf", height=7, width=11)

d <- ggplot(y, aes(PC1, PC2))
d + geom_point(size = 2) + theme_bw(base_size = 12) + xlab(x_lab) + ylab(y_lab)

dev.print(pdf, file = "loadings.pdf", height=7, width=11)
