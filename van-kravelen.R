library(ggplot2)
library(qdap)
library(dplyr)
library(gridExtra)
df <- read.csv("formula.csv")

df$formula <- gsub("([0-9])([A-Z])", "\\1~\\2", df$formula)
df$formula <- gsub("([A-Z])([A-Z])", "\\1~\\2", df$formula)
df$formula <- gsub("([A-Z])([A-Z])", "\\1~\\2", df$formula)
df$formula <- paste0(df$formula, "~")
df$formula <- gsub("([A-Z])(~)", "\\11\\2", df$formula)
df$formula <- gsub("(Cl)", "X", df$formula)
df$formula <- gsub("(Hg)", "X", df$formula)

df$C <- as.numeric(genXtract(df$formula, "C", "~"))
df$H <- as.numeric(genXtract(df$formula, "H", "~"))
df$O <- as.numeric(genXtract(df$formula, "O", "~"))
df$N <- as.numeric(genXtract(df$formula, "N", "~"))
df$S <- as.numeric(genXtract(df$formula, "S", "~"))
df[is.na(df)] <- 0
df$DBE <- df$C - (df$H/2) + (df$N/2) + 1

df.m <- read.csv("positive_featurelist2.csv")
#create names
if("name" %in% colnames(df.m)){
        cat("names were defined\n")
} else{
        df.m$name <- paste("M", round(df.m$mz, 3), "T", round(df.m$rt, 3), sep = "")        
}

dfx <- filter(df.m, grepl("\\[M\\]\\+", isotopes))
dfx$iso.no <- gsub("[^0-9]", "", dfx$isotopes)
df.b <- merge(df,dfx, by = "iso.no")

df.S <- filter(df.b, S > 0)
write.csv(df.S, "S_containing.csv")


df.b$OC <- df.b$O/df.b$C
df.b$HC <- df.b$H/df.b$C

df.b$MW2 <- apply(select(df.b, MW2a:MW2c), 1, function(x){
        log2(mean(x[1:3]))
})

df.b$MW4 <- apply(select(df.b, MW4a:MW4c), 1, function(x){
        log2(mean(x[1:3]))
})

df.b$MW8 <- apply(select(df.b, MW8a:MW8c), 1, function(x){
        log2(mean(x[1:3]))
})

df.b$MW5 <- apply(select(df.b, MW5a:MW5c), 1, function(x){
        log2(mean(x[1:3]))
})

p1 <- ggplot(df.b, aes(df.b$OC, df.b$HC, colour = MW2)) +
        geom_point(size = 3) +
        scale_color_gradientn(colours = rainbow(7)) +
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0, 2)) +
        xlab("O/C") +
        ylab("H/C") +
        ggtitle("MW2") +
        theme_bw(base_size = 22, base_family = "georgia")

p2 <- ggplot(df.b, aes(df.b$OC, df.b$HC, colour = MW5)) +
        geom_point(size = 3) +
        scale_color_gradientn(colours = rainbow(7)) +
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0, 2)) +
        xlab("O/C") +
        ylab("H/C") +
        ggtitle("MW5") +
        theme_bw(base_size = 22, base_family = "georgia")

p3 <- ggplot(df.b, aes(df.b$OC, df.b$HC, colour = MW8)) +
        geom_point(size = 3) +
        scale_color_gradientn(colours = rainbow(7)) +
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0, 2)) +
        xlab("O/C") +
        ylab("H/C") +
        ggtitle("MW8") +
        theme_bw(base_size = 22, base_family = "georgia")

grid.arrange(p1, p2, p3)


