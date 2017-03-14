df <- read.csv("AN.csv")
rownames(df) <- df$samples
df$samples <- NULL
df[df==0] <- 0.0000000000001
df15norm <- log(df[2,]/df[1,])
df62norm <- log(df[3,]/df[1,])
rownames(df15norm) <- "15 ppm"
rownames(df62norm) <- "62 ppm"
norm <- rbind(df15norm, df62norm)
norm$samples <- rownames(norm)
m.D <- reshape2::melt(norm, id=c("samples"))
m.D$variable <- gsub("X", "", m.D$variable)
library(ggplot2)
g1 <- ggplot(m.D, aes(m.D$value, as.integer(m.D$variable))) +
        geom_jitter(aes(color = samples))+
        xlab("log (Fold change)")+
        ylab("Mass")+
        theme_bw(base_size = 22, base_family = "Times")
g1
dev.print(png, "AN_foldchange.pdf", height = 10, width = 10)
m.D2 <- dplyr::filter(m.D, value < -10)
df2 <- read.csv("AN_transpose.csv")
"%ni%" <- Negate("%in%")
down <- dplyr::filter(df2, mz %in% m.D2$variable)
up <- dplyr::filter(df2, mz %ni% m.D2$variable)
updown <- rbind(down, up)
write.csv(updown, "AN2_2.csv")
