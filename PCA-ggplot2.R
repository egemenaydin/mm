data <- read.csv("PoE_all_raw_nonaveraged.csv", header = TRUE)

rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

require(caret)
require(ggplot2)

trans <- preProcess(data[ ,1:length(data)], method=c("BoxCox", "center", "scale", "pca"))
PC <- predict(trans, data[ ,1:length(data)])
x <- trans$rotation
write.csv(x, file = "PCA.csv")

y <- read.csv("PCA.csv", row.names = 1)

qplot(PC1, PC2, data = y, color = row.names(y))

dev.print(pdf, file = "PCA.pdf", height=7, width=11)