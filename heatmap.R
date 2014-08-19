require(gplots)

data <- read.csv("air-sparge-all-log-2-no-mq.csv")
rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

pdf("heatmap-all-5.pdf")
heatmap.2(as.matrix(data), Rowv = FALSE, Colv = TRUE, 
          dendrogram = "column", col = redblue(16), 
          key = T, keysize = 1.5, density.info = "none", 
          trace = "none", labRow = NA)
dev.off()
