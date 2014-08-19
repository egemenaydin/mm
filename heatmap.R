require(gplots)
require(RColorBrewer)

data <- read.csv("air-sparge-S-log-2-no-mq.csv")
rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

pdf("heatmap-S.pdf")
heatmap.2(as.matrix(data), Rowv = FALSE, Colv = TRUE, 
          dendrogram = "column", col = brewer.pal(8, "Reds"), 
          key = T, keysize = 1.5, density.info = "none", 
          trace = "none", labRow = NA)
dev.off()
