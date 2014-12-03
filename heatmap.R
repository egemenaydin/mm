require(gplots)
require(RColorBrewer)

data <- read.csv("sample2-transposed.csv", check.names = FALSE)
rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

data_polished <- data[, apply(data, 2, var, na.rm = TRUE) != 0]
data_polished[data_polished == 0] <- 1

log.data <- data.frame(log2(data_polished[ , 1:length(data)]), check.names = FALSE)


cols <- colorRampPalette(c("black","red"))(75)

heatmap.2(as.matrix(data), Rowv = TRUE, Colv = TRUE, 
          dendrogram = "column", col = cols, scale = c("row"),
          key = T, key.title = NULL, keysize = 2, density.info = "none", 
          trace = "none", labRow = NA)
dev.print(pdf, "heatmap-below-400.pdf", height = 5, width = 10)
