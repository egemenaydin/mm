data <- read.csv("air-sparge-2.csv")

data[data == 0] <- -9

data.frame(data[1:2893, ], data$p <- apply(data[1:2893, ], 1, function(x) {
        t.test(x[4:6], x[22:24], paired = TRUE)$p.value
} ))

data2 <- read.csv("air-sparge.csv")

comp_name <- data2[, 1]

cbind(comp_name, data)

comp <- na.omit(data)

data.frame(comp[1:1305, ], comp$log_p <- apply(comp[1:1305, ], 1, function(y){
        -log10(y[7])
}))

data.frame(comp[1:1305, ], comp$dif <- apply(comp[1:1305, ], 1, function(z){
        mean(z[4:6] - z[1:3], na.rm = TRUE)
}))

comp$threshold <- as.factor(abs(comp$dif) > 2 & comp$log_p > 2)

with(comp, plot(dif, log_p), xlab = "log2 Fold Change", ylab = "-log10(P)", pch = 20)

x <- subset(data, A1100a == 5)

with(subset(comp, comp$threshold == "TRUE", points(dif, log_p, col = "red"), , pch = 20))
with(subset(comp$threshold == "FALSE", points(dif, log_p, col = "black"), , pch = 20))

require(ggplot2)
##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off


##Construct the plot object
g = ggplot(data=comp, aes(x=dif, y=log_p), (colour=threshold) +
        geom_point(alpha=0.4, size=1.75) +
        opts(legend.position = "none") +
        xlim(c(-10, 10)) + ylim(c(0, 15)) +
        xlab("log2 fold change") + ylab("-log10 p-value")
g
