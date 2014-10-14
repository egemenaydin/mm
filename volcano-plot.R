data <- read.csv("PoE_1095CandD_raw_nonaveraged.csv")

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

all_false <- subset(df, df$increased == "FALSE" & df$decreased == "FALSE")

par(xpd = TRUE, mar = par()$mar + c(0, 5, 0, 0))

with(df, plot(dif, log_p, xlab = "log2 Fold Change", ylab = "-log10(P)", pch = 20, col = "skyblue"))

with(t1, points(dif, log_p, col = "maroon", pch = 20))

with(t2, points(dif, log_p, col = "golden rod", pch = 20))

legend(-38, 6, xpd= TRUE,  pch = 20, col = c("maroon", "golden rod"), legend = c("Decreased", "Increased"), bty = "n")

dev.print(pdf, "1095CandD.pdf", height=5, width=8.5)

write.csv(t1, "1095CandD_decreased.csv")

write.csv(t2, "1095CandD_increased.csv")

write.csv(all_false, "1095CandD_unchanged.csv")
