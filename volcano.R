data <- read.csv("air-sparge-2.csv")

data[data == 0] <- -9

data.frame(data[1:2893, ], data$p <- apply(data[1:2893, ], 1, function(x) {
        t.test(x[4:6], x[22:24], paired = TRUE)$p.value
} ))

data.frame(data[1:2893, ], data$log_p <- apply(data[1:2893, ], 1, function(y){
        -log10(y[25])
}))

data.frame(data[1:2893, ], data$dif <- apply(data[1:2893, ], 1, function(z){
        mean(z[4:6] - z[22:24], na.rm = TRUE)
}))

data2 <- read.csv("air-sparge.csv")

data$compound <- data2[, 1]

data$threshold <- as.factor(abs(data$dif) > 2 & data$log_p > 2)

df <- data.frame(data)

x <- subset(df, df$threshold == "TRUE")

y <- subset(df, df$threshold == "FALSE")

with(df, plot(dif, log_p, xlab = "log2 Fold Change", ylab = "-log10(P)", pch = 20))

with(x, points(dif, log_p, col = "red", pch = 20))


>>>>>>> bd621aca86a402c44600c1fcc526256cb8e9047d

require(ggplot2)
##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off


##Construct the plot object
g = ggplot(data=comp, aes(x=dif, y=log_p), (colour=threshold) +
        geom_point(alpha=0.4, size=1.75) +
        opts(legend.position = "none") +
        xlim(c(-10, 10)) + ylim(c(0, 15)) +
        xlab("log2 fold change") + ylab("-log10 p-value")
g
