data <- read.csv("sample.csv", header = TRUE)

rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

require(caret)
require(ggplot2)

trans <- preProcess(data[ ,1:length(data)], method=c("BoxCox", "center", "scale", "pca"))
PC <- predict(trans, data[ ,1:length(data)])
x <- trans$rotation
write.csv(x, file = "PCA.csv")

y <- read.csv("PCA.csv", row.names = 1)

g <- ggplot(y, aes(PC1, PC2))

g + geom_point(aes(color = row.names(y)), size = 2.5) + theme_bw(base_size = 12, base_family = "Times")

dev.print(pdf, file = "PCA.pdf", height=7, width=11)
