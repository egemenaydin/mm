require(gplots)
require(RColorBrewer)

data <- read.csv("PoE-log2-S-nonaveraged.csv", check.names = FALSE)
rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

heatmap.2(as.matrix(data), Rowv = FALSE, Colv = TRUE, 
          dendrogram = "column", col = bluered(16), 
          key = T, key.title = NULL, keysize = 1.2, density.info = "none", 
          trace = "none", labRow = NA)
dev.print(pdf, "heatmap-S.pdf", height = 5, width = 10)
