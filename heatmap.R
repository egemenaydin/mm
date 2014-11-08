require(gplots)
require(RColorBrewer)

data <- read.csv("raw-data-log2-below-400-no-mass.csv", check.names = FALSE)
rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

heatmap.2(as.matrix(data), Rowv = FALSE, Colv = TRUE, 
          dendrogram = "column", col = brewer.pal(8, "Reds"), 
          key = T, key.title = NULL, keysize = 1.2, density.info = "none", 
          trace = "none", labRow = NA)
dev.print(pdf, "heatmap-below-400.pdf", height = 5, width = 10)
