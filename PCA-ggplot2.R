data <- read.csv("PoE_all_raw_nonaveraged.csv", header = TRUE)

rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

require(caret)
require(RColorBrewer)
require(ggplot2)
trans <- preProcess(data[ ,1:length(data)], method=c("BoxCox", "center", "scale", "pca"))
PC <- predict(trans, data[ ,1:length(data)])
x <- trans$rotation
write.csv(x, file = "PCA.csv")

y <- read.csv("PCA.csv", row.names = 1)
y$sample <- y$row.names
qplot(PC1, PC2, data = y, color = row.names(y))

with(y, plot(PC1, PC2, pch = 20))

f <- gl(nrow(y)/3, 3)
s <- split(y, f)
z <-data.frame(s)
c <- brewer.pal(11, "Spectral")

sapply(s , with(s, points(PC1, PC2, col = "c", pch = 20, cex = 1.7)))

with(y[1:3, ], points(PC1, PC2, col = "salmon", pch = 20, cex = 1.7))
with(y[4:6, ], points(PC1, PC2, col = "forestgreen", pch = 20, cex = 1.7))
with(y[7:9, ], points(PC1, PC2, col = "darkolivegreen1", pch = 20, cex = 1.7))
with(y[10:12, ], points(PC1, PC2, col = "orange", pch = 20, cex = 1.7))
with(y[13:15, ], points(PC1, PC2, col = "darkorange", pch = 20, cex = 1.7))
with(y[16:18, ], points(PC1, PC2, col = "cornsilk2", pch = 20, cex = 1.7))
with(y[19:21, ], points(PC1, PC2, col = "cornsilk4", pch = 20, cex = 1.7))
with(y[22:24, ], points(PC1, PC2, col = "yellow", pch = 20, cex = 1.7))
with(y[25:27, ], points(PC1, PC2, col = "yellow4", pch = 20, cex = 1.7))
with(y[28:30, ], points(PC1, PC2, col = "navyblue", pch = 20, cex = 1.7))
with(y[31:33, ], points(PC1, PC2, col = "deepskyblue", pch = 20, cex = 1.7))
with(y[34:36, ], points(PC1, PC2, col = "plum1", pch = 20, cex = 1.7))
with(y[37:39, ], points(PC1, PC2, col = "plum4", pch = 20, cex = 1.7))
with(y[40:42, ], points(PC1, PC2, col = "turquoise", pch = 20, cex = 1.7))
with(y[43:45, ], points(PC1, PC2, col = "steelblue4", pch = 20, cex = 1.7))
with(y[46:48, ], points(PC1, PC2, col = "red4", pch = 20, cex = 1.7))
with(y[49:51, ], points(PC1, PC2, col = "red", pch = 20, cex = 1.7))
with(y[52:54, ], points(PC1, PC2, col = "palevioletred1", pch = 20, cex = 1.7))


legend("bottomright", pch = 20, ncol = 2, col = c("cornsilk2", "cornsilk4", "plum1", "plum4", "yellow", "yellow4", "navyblue", "deepskyblue", "turquoise", "steelblue4", "red4", "red", "palevioletred1", "salmon", "forestgreen", "darkolivegreen1", "orange", "darkorange"), legend = c("14-NPR-PW-0161A-DW", "14-NPR-PW-0161D-DW", "14-NPR-PW-0511A-DW", "14-NPR-PW-0511D3-DW", "14-NPR-PW-0217A-DW", "14-NPR-PW-0217D-DW", "14-NPR-PW-1230-GW", "14-NPR-MW-130-GW", "14-NPR-MW-304-15-GW", "14-NPR-MW-164B-GW", "14-NPR-PW-1095A-DW", "14-NPR-PW-1095B-DW", "14-NPR-PW-1095C1-DW", "14-NPR-PW-1095D-DW", "14-NPR-PW-0607A-DW", "14-NPR-PW-0607D-DW", "14-NPR-PW-1100A-DW", "14-NPR-PW-1100D-DW"), pt.cex=1, cex=0.7)
dev.print(pdf, file = "PoE-PCA-2.pdf", height=7, width=11)

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

