data <- read.csv("1100-log2-triplicates-nocomp.csv")

data[data == 0] <- -9

data.frame(data[1:2991, ], data$p <- apply(data[1:2991, ], 1, function(x) {
        t.test(x[1:3], x[4:6], paired = TRUE)$p.value
} ))

data.frame(data[1:2991, ], data$log_p <- apply(data[1:2991, ], 1, function(y){
        -log10(y[7])
}))

data.frame(data[1:2991, ], data$dif <- apply(data[1:2991, ], 1, function(z){
        mean(z[1:3] - z[4:6], na.rm = TRUE)
}))

data2 <- read.csv("1100-log2-triplicates.csv")

data$compound <- data2[, 1]

data$upreg <- as.factor(data$dif > 2 & data$log_p > 2)

df <- data.frame(data)

t1 <- subset(df, df$upreg == "TRUE")

f1 <- subset(df, df$upreg == "FALSE")

df$downreg <- as.factor(data$dif < -2 & data$log_p > 2)

t2 <- subset(df, df$downreg == "TRUE")

f2 <- subset(df, df$downreg == "FALSE")

with(df, plot(dif, log_p, xlab = "log2 Fold Change", ylab = "-log10(P)", pch = 20))

with(t1, points(dif, log_p, col = "red", pch = 20))

with(t2, points(dif, log_p, col = "blue", pch = 20))


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
