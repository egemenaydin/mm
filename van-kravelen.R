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
#read phenodata
PhD <- data.frame(read.csv("PhDP.csv"))
fNames <- as.vector(PhD$X)
fNames <- gsub("-", "\\.", fNames)
fSamples <- as.vector(PhD$class)
fSamples <- gsub("-", "\\.", fSamples)
grs <- unique(fSamples)

dfx <- filter(df.m, grepl("\\[M\\]\\+", isotopes))
dfx$iso.no <- gsub("[^0-9]", "", dfx$isotopes)
df.b <- merge(df,dfx, by = "iso.no")

df.S <- filter(df.b, S > 0)
write.csv(df.S, "S_containing.csv")


df.b$OC <- df.b$O/df.b$C
df.b$HC <- df.b$H/df.b$C

for (i in length(grs)) {
        df.b[[paste(grs[[i]])]] <- apply(df.b[,grepl(grs[[i]], colnames(df.b))], 1, mean)
}

df.b$min <- apply(select(df.b, MW1:MW8), 1, min)
df.b$max <- apply(select(df.b, MW1:MW8), 1, max)
df.b$mean <- apply(select(df.b, MW1:MW8), 1, mean)

df.b$normMW2 <- -1+2*(df.b$MW2-df.b$min)/(df.b$max-df.b$min)
df.b$normMW8 <- -1+2*(df.b$MW8-df.b$min)/(df.b$max-df.b$min)



p1 <- ggplot(df.b, aes(df.b$OC, df.b$HC, colour = normMW2)) +
        geom_point(size = 3) +
        scale_color_gradientn(colours = rainbow(7)) +
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0.3, 2)) +
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

p3 <- ggplot(df.b, aes(df.b$OC, df.b$HC, colour = normMW8)) +
        geom_point(size = 3) +
        scale_color_gradientn(colours = rainbow(7)) +
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0.3, 2)) +
        xlab("O/C") +
        ylab("H/C") +
        ggtitle("MW8") +
        theme_bw(base_size = 22, base_family = "georgia")

grid.arrange(p1, p2, p3)


