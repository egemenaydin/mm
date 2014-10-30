library("mixOmics")
data <- read.csv("PoE_all_raw_nonaveraged_transposed4.csv", check.names = FALSE, row.names = 1)
x <- as.matrix(data[ , 2:length(data)])
y <- data$type
result <- plsda(x, y, ncomp = 3)
palette(c("red", "blue", "black"))
col.breast <- as.numeric(as.factor(y))
plotIndiv(result, ind.names = TRUE, pch = c(16, 16), col = col.breast)
legend('bottomleft', c("Untreated water", "Treated water", "Groundwater"), pch = c(16, 16),
       col = unique(col.breast), cex = 1, pt.cex = c(1.2, 1.2)
       )
palette("default")
