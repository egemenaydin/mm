data <- read.csv("try.csv", header = TRUE)
log.ir <- log(data[, 1:8])
ir.species <- data[, 1]
ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE) 
print(ir.pca)
write.csv(ir.pca[Rotation], file = "pca.csv")
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

require(caret)
trans = preProcess(data[,1:8],
                   method=c("BoxCox", "center",
                            "scale", "pca"))
PC = predict(trans, data[,1:8])
x <- trans$rotation
write.csv(x, file = "PCA.csv")

y <- read.csv("PCA.csv", row.names = 1)
with(y, plot(PC1, PC2), pch = 20)
with(y[1, ], points(PC1, PC2, col = "green", pch = 20))
with(y[2, ], points(PC1, PC2, col = "red", pch = 20))
with(y[3, ], points(PC1, PC2, col = "blue", pch = 20))
with(y[4, ], points(PC1, PC2, col = "orange", pch = 20))
with(y[5, ], points(PC1, PC2, col = "black", pch = 20))
with(y[6, ], points(PC1, PC2, col = "yellow", pch = 20))
with(y[7, ], points(PC1, PC2, col = "pink", pch = 20))
with(y[8, ], points(PC1, PC2, col = "purple", pch = 20))
legend("topright", pch = 20, ncol = 2, col = c("green", "red", "blue", "orange", "black", "yellow", "pink", "purple"), legend = c("MW1", "MW2", "MW3", "MW4", "MW5", "MW6", "MW7", "MW8"))
dev.copy(pdf, file = "airspargePCA.pdf")
dev.off()



library(corrplot)
#corrplot: the library to compute correlation matrix.

datMy <- read.table("data.csv", header = TRUE)
#read the tab file using the read table function.

datMy.scale<- scale(datMy[2:ncol(datMy)],center=TRUE,scale=TRUE);
#scale all the features (from feature 2 bacause feature 1 is the predictor output)

corMatMy <- cor(datMy.scale)
#compute the correlation matrix

corrplot(corMatMy, order = "hclust")
#visualize the matrix, clustering features by correlation index.

highlyCor <- findCorrelation(corMatMy, 0.70)
#Apply correlation filter at 0.70,
#then we remove all the variable correlated with more 0.7.
datMyFiltered.scale <- datMy.scale[,-highlyCor]
corMatMy <- cor(datMyFiltered.scale)
corrplot(corMatMy, order = "hclust")

require(FactoMineR) 
# PCA with function PCA

datMy <- read.table("data.csv", header = TRUE)
#read the tab file using the read table function.

pca <- PCA(datMy, scale.unit=TRUE, ncp=5, graph=T)
#scale all the features,  ncp: number of dimensions kept in the results (by default 5)

dimdesc(pca)
#This line of code will sort the variables the most linked to each PC. It is very useful when you have many variables.

