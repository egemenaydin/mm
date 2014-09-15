data <- read.csv("Bac_Abundance_Class_no_rarefaction.csv", header = TRUE)
data[data == 0] <- NA
rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Taxon <- NULL

require(caret)
trans <- preProcess(data[,1:12], method=c("BoxCox", "center", "scale", "pca"))
PC <- predict(trans, data[,1:12])
x <- trans$rotation
write.csv(x, file = "PCA.csv")

y <- read.csv("PCA.csv", row.names = 1)
with(y, plot(PC1, PC2, pch = 20))
with(y[1:2, ], points(PC1, PC2, col = "green", pch = 20, cex = 1.7))
with(y[3:4, ], points(PC1, PC2, col = "red", pch = 20, cex = 1.7))
with(y[5:6, ], points(PC1, PC2, col = "blue", pch = 20, cex = 1.7))
with(y[7:8, ], points(PC1, PC2, col = "orange", pch = 20, cex = 1.7))
with(y[9:10, ], points(PC1, PC2, col = "purple", pch = 20, cex = 1.7))
with(y[11:12, ], points(PC1, PC2, col = "yellow", pch = 20, cex = 1.7))
#with(y[19:21, ], points(PC1, PC2, col = "pink", pch = 20, cex = 1.7))
#with(y[22:24, ], points(PC1, PC2, col = "purple", pch = 20, cex = 1.7))

legend("bottomleft", pch = 20, ncol = 2, col = c("green", "red", "blue", "orange", "purple", "yellow"), legend = c("Brick B124 After (0 cm)", "Brick B124 Before (0 cm)", "Brick B124 After (16 cm)", "Brick B124 Before (16 cm)", "Wood B124 After", "Wood B124 Before"), pt.cex=1, cex=0.7)
dev.print(pdf, file = "bacteria-PCA.pdf", height=7, width=11)

----------------
        
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

----------------------

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

