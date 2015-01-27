if(!require("gplots")){
        install.packages("gplots", dependencies = TRUE)
        library(gplots)
}

if(!require("RColorBrewer")){
        install.packages("RColorBrewer", dependencies = TRUE)
        library(RColorBrewer)
}


data <- read.csv("sample2.csv", check.names = FALSE)
rnames <- data[ ,1]
data$sample <- NULL
data[data == 0] <- 0.5*(min(data[data>0],na.rm=TRUE))
log_data <- data.frame(log2(data[ , 1:length(data)]))
data_matrix <- data.matrix(log_data[ , 1:length(log_data)])
rownames(data_matrix) <- rnames

cols <- colorRampPalette(c("black","red"))(75)

heatmap.2(data_matrix, Rowv = TRUE, Colv = "NA", 
          dendrogram = "row", col = cols, scale = c("none"),
          key = T, key.title = NULL, keysize = 2, density.info = "none", 
          trace = "none", labCol = NA)
dev.print(pdf, "heatmap-below-400.pdf", height = 5, width = 10)
