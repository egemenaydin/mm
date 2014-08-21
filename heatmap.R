require(gplots)
require(RColorBrewer)

data <- read.csv("camelina-all-log2.csv")
rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL


heatmap.2(as.matrix(data), Rowv = FALSE, Colv = TRUE, 
          dendrogram = "column", col = bluered(16), 
          key = T, keysize = 1.5, density.info = "none", 
          trace = "none", labRow = NA)
dev.print(pdf, "heatmap-S-new-2.pdf", height = 6, width = 6)
