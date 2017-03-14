if(!require("gplots")){
        install.packages("gplots", dependencies = TRUE)
        library(gplots)
}

if(!require("RColorBrewer")){
        install.packages("RColorBrewer", dependencies = TRUE)
        library(RColorBrewer)
}


data <- read.csv("aromatic-HC-metabolites-Jamie-newnames-compound-names2.csv", check.names = FALSE)
rnames <- colnames(data)
cnames <- data$Compound
data$Compound <- NULL
data[data == 0] <- 0.3*(min(data[data>0],na.rm=TRUE))
log_data <- data.frame(log2(data[ , 1:length(data)]))
data_matrix <- data.matrix(log_data[ , 1:length(log_data)])
colnames(data_matrix) <- rnames[2:length(rnames)]
rownames(data_matrix) <- cnames[1:length(cnames)]
par(mar=c(10,4,4,2)+1)
cols <- colorRampPalette(c("green","red"))(16)

heatmap.2(data_matrix, Rowv = T, Colv = T, 
          dendrogram = "col", col = cols, scale = c("none"),
          key = F, key.title = "NULL", keysize = 1, density.info = "none", 
          cexCol = 0.75, lhei = c(1, 6), lwid = c (0.25, 4), 
          trace = "none", labRow = cnames , margins = c(5, 15))
dev.print(png, "heatmap.png", height = 5, width = 10, res = 600, unit = "in")
        
