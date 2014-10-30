library(ggbiplot)
data <- read.csv("sample2.csv", header = TRUE, check.names = FALSE)
rownames(data) <- make.names(data$Compound, unique = TRUE)
data$Compound <- NULL
data[data == 0] <- 2

log.data <- log(data[ , 2:length(data)])
samples <- data$Compound
pca <- prcomp(log.data, center = TRUE, scale. = TRUE)
plot(pca, type = "l")
summary(pca)
print(pca)
x <- predict(pca)
y <- pca$rotation
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = samples, ellipse = TRUE, circle = TRUE, var.axes = FALSE)
print(g)
