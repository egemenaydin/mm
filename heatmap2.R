if(!require("gplots")){
        install.packages("gplots", dependencies = TRUE)
        library(gplots)
}

if(!require("RColorBrewer")){
        install.packages("RColorBrewer", dependencies = TRUE)
        library(RColorBrewer)
}


data <- read.csv("feature_matrix_merged.csv", check.names = FALSE)
rnames <- data[ ,1]
data$samples <- NULL
#data[data == 0] <- 0.5*(min(data[data>0],na.rm=TRUE))
#log_data <- data.frame(log2(data[ , 1:length(data)]))
data_matrix <- data.matrix(data[ , 1:length(data)])
rownames(data_matrix) <- rnames

cols <- colorRampPalette(c("cyan","magenta"))(16)
png(file = "heatmap4.png", width = 16, height = 9, units = "in", res = 600)
heatmap.2(data_matrix, Rowv = "NA", Colv = "NA", 
          dendrogram = "none", col = cols, scale = c("column"),
          key = T, key.title = NA, keysize = 1, density.info = "hist", 
          trace = "none", labCol = NA, cexRow = 1.45, margins = c(3,15), 
          xlab = "m/z, between 80-4000 Da on increasing order")
dev.off()

dev.print(pdf, "heatmap.pdf", height = 5, width = 10)
