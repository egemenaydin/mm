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



log.data <- data.frame(log2(data_polished[ , 1:length(data)]), check.names = FALSE)


cols <- colorRampPalette(c("black","red"))(75)

heatmap.2(as.matrix(data), Rowv = TRUE, Colv = TRUE, 
          dendrogram = "column", col = cols, scale = c("row"),
          key = T, key.title = NULL, keysize = 2, density.info = "none", 
          trace = "none", labRow = NA)
dev.print(pdf, "heatmap-below-400.pdf", height = 5, width = 10)
