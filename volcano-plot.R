data <- read.csv("217-log-2-triplicates-nocomp.csv")

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

data2 <- read.csv("217-log-2-triplicates-nocomp.csv")

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
