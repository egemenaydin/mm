library(ggplot2)
library(RColorBrewer)
data <- read.csv("fold-change-1095a.csv", header = TRUE, check.names = FALSE)

data_polished <- data[data[1] != 1 | data[2] != 1 | data[3] != 1 | data[4] != 1 | data[5] != 1 | data[6] != 1, ]

data_polished[data_polished == 1] <- 0.5*(min(data_polished[data_polished>1100],na.rm=TRUE))

data.frame(data_polished, data_polished$average1 <- apply(data_polished, 1, function(x){
        mean(x[1:3])
}))

data.frame(data_polished, data_polished$SD1 <- apply(data_polished, 1, function(x){
        sd(x[1:3])
}))

data.frame(data_polished, data_polished$RSD1 <- apply(data_polished, 1, function(x){
        x[9]*100/x[8]
}))

data.frame(data_polished, data_polished$average2 <- apply(data_polished, 1, function(x){
        mean(x[4:6])
}))

data.frame(data_polished, data_polished$SD2 <- apply(data_polished, 1, function(x){
        sd(x[4:6])
}))

data.frame(data_polished, data_polished$RSD2 <- apply(data_polished, 1, function(x){
        x[12]*100/x[11]
}))

df <- data.frame(data_polished)

fold1 <- subset(df, RSD1 < 70 & RSD2 < 70)

data.frame(fold1, fold1$fold_change <- apply(fold1, 1, function(x){
        log2(x[11]) - log2(x[8])
}))


g <- ggplot(fold1, aes(fold_change, Mass))
g + geom_point(aes(color = "Marooon"), size = 3) + theme_bw(base_size = 16) + geom_abline(intercept = 400, slope = 0, color = "darkgreen", size = 2) + geom_vline(xintercept = 0, color = "darkgreen", size = 2) + theme(legend.position = "none")
