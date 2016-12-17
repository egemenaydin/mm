library(ggplot2)
library(RColorBrewer)
data <- read.csv("all_ions_for_PCA.csv", header = TRUE)

rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

require(caret)
require(ggplot2)

trans <- preProcess(dataAll_PCA, method=c("BoxCox", "center", "scale", "pca"))
PC <- predict(trans, dataAll_PCA)
x <- trans$rotation
write.csv(PC, file = "PCA_caret.csv")
write.csv(x, file = "loadings_caret.csv")

y <- read.csv("PCA.csv", row.names = 1)

g <- ggplot(PC, aes(PC1, PC2))

g + geom_point(aes(color = row.names(PC)), size = 2.5) + theme_bw(base_size = 12, base_family = "Times")

dev.print(pdf, file = "PCA.pdf", height=7, width=11)
