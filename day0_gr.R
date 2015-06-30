library(dplyr)
library(ggplot2)

t0 <- read.csv("Camelina_mb_t0.csv")
t2 <- read.csv("Camelina_mb_t2.csv")

t0t2 <- rbind(t0, t2)
t0t2 <- distinct(t0t2, X)

rn <- t0t2$X

baseC <- read.csv("base_in_feature_extract.csv")
baseC <- slice(baseC, rn)
baseC <- select(baseC, mz, rt, FE3.tp0.a:FE3.tp4.b)
colnames(baseC) <- c("mz", "rt", "Day0a", "Day0b", "Day20a", "Day20b", "Day40a", "Day40b")

baseC <- mutate(baseC, Day0 = Day0a+Day0b/2)
baseC <- mutate(baseC, Day20 = Day20a+Day20b/2)
baseC <- mutate(baseC, Day40 = Day40a+Day40b/2)

baseC <- mutate(baseC, logDay0 = log2(Day0))
baseC <- mutate(baseC, logDay20 = log2(Day20))
baseC <- mutate(baseC, logDay40 = log2(Day40))

baseC$mass <- baseC$mz - 1.007276

ggplot(baseC, aes(x = rt, y = mass, color = logDay0, size = logDay0)) +
        geom_point(position = "jitter") +
        scale_size("logDay0", guide = FALSE) +
        scale_colour_gradient(low = "lightgreen", high ="red", name = "Norm. abundance") +
        theme_bw(base_size = 16) +
        xlab("Retention time (sec)") +
        ylab("Mass (Da)") +
        ggtitle ("Day0")

dev.print(pdf, "Camelina-tp0.pdf", height = 6, width = 9)

ggplot(baseC, aes(x = rt, y = mass, color = logDay20, size = logDay20)) +
        geom_point(position = "jitter") +
        scale_size("logDay20", guide = FALSE) +
        scale_colour_gradient(low = "lightgreen", high ="red", name = "Norm. abundance") +
        theme_bw(base_size = 16) +
        xlab("Retention time (sec)") +
        ylab("Mass (Da)") +
        ggtitle("Day 20")

dev.print(pdf, "Camelina-tp1.pdf", height = 6, width = 9)

ggplot(baseC, aes(x = rt, y = mass, color = logDay40, size = logDay40)) +
        geom_point(position = "jitter") +
        scale_size("logDay40", guide = FALSE) +
        scale_colour_gradient(low = "lightgreen", high ="red", name = "Norm. abundance") +
        theme_bw(base_size = 16) +
        xlab("Retention time (sec)") +
        ylab("Mass (Da)") +
        ggtitle("Day40")

dev.print(pdf, "Camelina-tp2.pdf", height = 6, width = 9)

df <- read.csv("FTF76-t0.csv")

df$mass <- df$mz - 1.007276

df$F <- apply(select(df, F1:F3), 1, function(x){
        mean(x)
})

df$logF <- log2(df$F)

df$norF <- apply(select(df, logF), 1, function(x) {
        -1+2*(x-min(df$logF))/(max(df$logF)-min(df$logF))
})


ggplot(df, aes(x = rt, y = mass, color = logF, size = logF)) +
        geom_point(position = "jitter") +
        scale_size("logF", guide = FALSE) +
        scale_colour_gradient(low = "lightgreen", high ="red", name = "Norm. abundance") +
        theme_bw() +
        xlab("Retention time (sec)") +
        ylab("Mass (Da)")

dev.print(pdf, "FTF76-tp0.pdf", height = 6, width = 9)

