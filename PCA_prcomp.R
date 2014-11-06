library(ggplot2)
data <- read.csv("sample2.csv", header = TRUE, check.names = FALSE)
data2 <- read.csv("sample2.csv", header = TRUE, check.names = FALSE)
rownames(data) <- make.names(data$Compound, unique = TRUE)
data$Compound <- NULL

data_polished <- data[, apply(data, 2, var, na.rm = TRUE) != 0]
data_polished[data_polished == 1] <- 0.5*(min(data_polished[data_polished>0],na.rm=TRUE))

log.data <- log(data_polished[ , 2:length(data_polished)])

Samples <- data2$Compound
pca <- prcomp(log.data, center = TRUE, scale. = TRUE)

PC <- predict(pca)
x <- write.csv(PC, file = "PCA.csv")
y <- read.csv("PCA.csv", row.names = 1)
summary <- summary(pca)
loadings <- pca$rotation

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

xlab <- sprintf ('PC1 (%0.1f%%)', 100*pca$sdev[1]^2/sum(pca$sdev^2))

g <- ggplot(y, aes(PC1, PC2))
g + geom_point(aes(color = Samples), size = 3) + theme_bw(base_size = 12)


dev.print(pdf, file = "PCA.pdf", height=7, width=11)