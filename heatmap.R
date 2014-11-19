require(gplots)
require(RColorBrewer)

data <- read.csv("air-sparge-all-raw-mass-ordered-no-mass-log2.csv", check.names = FALSE)
rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL
log.data <- data.frame(log2(data[ , 1:length(data)]), check.names = FALSE)

cols <- colorRampPalette(c("white","red"))(16)

heatmap.2(as.matrix(data), Rowv = FALSE, Colv = TRUE, 
          dendrogram = "column", col = cols, 
          key = T, key.title = NULL, keysize = 2, density.info = "none", 
          trace = "none", labRow = NA)
dev.print(pdf, "heatmap-below-400.pdf", height = 5, width = 10)
