library(dplyr)
data <- read.csv("base_in_feature_extract_vol_mb_t0.csv", check.names = FALSE)

#rownames(data) <- make.names(data[, 1], unique = TRUE)
#data$mz <- NULL
data[is.na(data)] <- 1
data[data == 0] <- 1

data_polished <- data[rowSums(data) != ncol(data), ]
data_polished[data_polished == 1] <- 0.5*(min(data_polished[data_polished>1],na.rm=TRUE))

data_polished$p <- apply(select(data_polished, F1:S3), 1, function(x) {
        t.test(x[1:3], x[4:6], paired = TRUE)$p.value
} )

data_polished$log_p <- -log10(data_polished$p)

data_polished$dif <- apply(select(data_polished, F1:S3), 1, function(z) {
        log2(mean(z[4:6], na.rm = TRUE) / mean(z[1:3], na.rm = TRUE))
})

data_polished$decreased <- as.factor(data_polished$dif < -2 & data_polished$log_p > 1.5)

df <- data.frame(data_polished)

t1 <- subset(df, df$decreased == "TRUE")

f1 <- subset(df, df$decreased == "FALSE")

df$increased <- as.factor(data_polished$dif > 2 & data_polished$log_p > 1.5)

t2 <- subset(df, df$increased == "TRUE")

f2 <- subset(df, df$increased == "FALSE")

all_false <- subset(df, df$increased == "FALSE" & df$decreased == "FALSE")

#par(xpd = TRUE, mar = par()$mar + c(0, 5, 0, 0))

with(df, plot(dif, log_p, xlab = "log2 Fold Change", ylab = "-log10(P)", pch = 20, col = "skyblue", cex = 0.5))

with(t1, points(dif, log_p, col = "maroon", pch = 20, cex = 0.5))

with(t2, points(dif, log_p, col = "golden rod", pch = 20, cex = 0.5))

#legend(-38, 6, xpd= TRUE,  pch = 20, col = c("maroon", "golden rod"), legend = c("Decreased", "Increased"), bty = "n")

dev.print(pdf, "Camelina_mb_t0_fc4.pdf", height=5, width=8.5)

write.csv(t1, "Camelina_mb_t0.csv")

write.csv(t2, "FTF76-t4.csv")

write.csv(all_false, "217AandD_unchanged.csv")
