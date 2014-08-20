data <- read.csv("camelina-raw.csv")

rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

data.frame(data, data$p <- apply(data, 1, function(x) {
        t.test(x[1:3], x[4:6], paired = TRUE)$p.value
} ))

data.frame(data, data$log_p <- apply(data, 1, function(y){
        -log10(y[7])
}))

data.frame(data, data$dif <- apply(data, 1, function(z){
        log2(mean(z[4:6], na.rm = TRUE) / mean(z[1:3], na.rm = TRUE))
}))

data$decreased <- as.factor(data$dif < -1 & data$log_p > 2)

df <- data.frame(data)

t1 <- subset(df, df$decreased == "TRUE")

f1 <- subset(df, df$decreased == "FALSE")

df$increased <- as.factor(data$dif > 1 & data$log_p > 2)

t2 <- subset(df, df$increased == "TRUE")

f2 <- subset(df, df$increased == "FALSE")

with(df, plot(dif, log_p, xlab = "log2 Fold Change", ylab = "-log10(P)", pch = 20))

with(t1, points(dif, log_p, col = "blue", pch = 20))

with(t2, points(dif, log_p, col = "red", pch = 20))

legend("top", pch = 20, col = c("blue", "red"), legend = c("decreased", "increased"), bty = "n")

dev.print(pdf, "cam-first-end2.pdf", height=5, width=5)
