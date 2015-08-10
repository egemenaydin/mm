library(ggplot2)
library(qdap)
library(dplyr)
library(gridExtra)
df <- read.csv("formula.csv")

df$Formula <- gsub("([0-9])([A-Z])", "\\1~\\2", df$Formula)
df$Formula <- gsub("([A-Z])([A-Z])", "\\1~\\2", df$Formula)
df$Formula <- gsub("([A-Z])([A-Z])", "\\1~\\2", df$Formula)
df$Formula <- paste0(df$Formula, "~")
df$Formula <- gsub("([A-Z])(~)", "\\11\\2", df$Formula)
df$Formula <- gsub("(Cl)", "X", df$Formula)
df$Formula <- gsub("(Hg)", "X", df$Formula)

df$C <- as.numeric(genXtract(df$Formula, "C", "~"))
df$H <- as.numeric(genXtract(df$Formula, "H", "~"))
df$O <- as.numeric(genXtract(df$Formula, "O", "~"))
df$N <- as.numeric(genXtract(df$Formula, "N", "~"))
df[is.na(df)] <- 0
df$DBE <- df$C - (df$H/2) + (df$N/2) + 1



df$OC <- df$O/df$C
df$HC <- df$H/df$C

df$tp0 <- apply(select(df, FE1.tp0.a, FE1.tp0.b), 1, function(x){
        log2(mean(x[1:2]))
})

df$tp1 <- apply(select(df, FE1.tp2.a, FE1.tp2.b), 1, function(x){
        log2(mean(x[1:2]))
})

df$tp2 <- apply(select(df, FE1.tp4.a, FE1.tp4.b), 1, function(x){
        log2(mean(x[1:2]))
})

p1 <- ggplot(df, aes(df$HC, df$OC, colour = tp0)) +
        geom_point(size = 4) +
        scale_color_gradient2() +
        xlab("H/C") +
        ylab("O/C") +
        ggtitle("Camelina day 0") +
        theme_bw()

p2 <- ggplot(df, aes(df$HC, df$OC, colour = tp1)) +
        geom_point(size = 4) +
        scale_color_gradient2() +
        xlab("H/C") +
        ylab("O/C") +
        ggtitle("Camelina day 20") +
        theme_bw()

p3 <- ggplot(df, aes(df$HC, df$OC, colour = tp2)) +
        geom_point(size = 4) +
        scale_color_gradient2() +
        xlab("H/C") +
        ylab("O/C") +
        ggtitle("Camelina day 40") +
        theme_bw()

grid.arrange(p1, p2, p3)