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
df$P <- as.numeric(genXtract(df$formula, "P", "~"))
df$S <- as.numeric(genXtract(df$formula, "S", "~"))

df[is.na(df)] <- 0
#df$DBE2 <- df$C - (df$H/2) + (df$N/2) + 1

df.m <- read.csv("positive_featurelist.csv")
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
df.b <- df.b[, !names(df.b) %in% grs[]]

df.b$OC <- df.b$O/df.b$C
df.b$HC <- df.b$H/df.b$C
df.b$CHO <- (2*df.b$O - df.b$H)/df.b$C

df.S <- filter(df.b, S > 0)
df.S$CHO <- (2*df.S$O - df.S$H)/df.S$C
df.S$formula <- gsub("~", "", df.S$formula)
write.csv(df.S, "S_containing.csv")

df.N <- filter(df.b, N > 0)
df.N$CHO <- (2*df.N$O - df.N$H)/df.N$C
df.N$formula <- gsub("~", "", df.N$formula)
df.N$NO <- paste("N", df.N$N, "O", df.N$O, sep = "")
df.N$NO <- gsub("1", "", df.N$NO)
write.csv(df.N, "N_containing.csv")
df.N.sel <- filter(df.N, NO == "NO" | NO == "NO2" |NO == "NO3" |NO == "N2O" |NO == "N2O2")
#df.N.sel <- filter(df.N.sel, df.N.sel$P == 0)
#df.N.sel <- filter(df.N.sel, df.N.sel$S == 0)


for (i in 1:length(grs)) {
        df.b[[paste(grs[[i]])]] <- apply(df.b[,grepl(grs[[i]], colnames(df.b))], 1, mean)
}

# df.b$min <- apply(select(df.b, MW1:MW8), 1, min)
# df.b$max <- apply(select(df.b, MW1:MW8), 1, max)
# df.b$mean <- apply(select(df.b, MW1:MW8), 1, mean)
# 
# df.b$normMW2 <- -1+2*(df.b$MW2-df.b$min)/(df.b$max-df.b$min)
# df.b$normMW8 <- -1+2*(df.b$MW8-df.b$min)/(df.b$max-df.b$min)

CHO <- data.frame(a = c(0, 0.5, 1, 1.5, 2, 2.5), 
                  b = c(1, 1, 1, 1, 1, 1), 
                  c = c("CHO = 0", "CHO = -0.5", "CHO = -1", "CHO = -1.5", "CHO = -2", "CHO = -2.5"), 
                  x = c(0.4, 0.4, 0.4, 0.4, 0.4, 0.4),
                  y = c(0.45, 0.95, 1.45, 1.95, 2.45, 2.95)
                  )

p1 <- ggplot(df.N.sel, aes(df.N.sel$OC, df.N.sel$HC, colour = NO)) +
        #geom_point(size = 3) +
        #scale_color_gradient(low = "cyan", high = "magenta", expression("log10(Abundance)")) +
        #geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=0.7,ymax=1.5), fill="#FFFFFF", alpha = 0.0008) +
        #geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=1,ymax=2), fill="#FFFFFF", alpha = 0.0008) +
#         geom_rect(data = NULL, aes(xmin=0.0,xmax=0.3,ymin=1.5,ymax=2), color = "black", fill="#FFFFFF", alpha = 0.0008) +
#         geom_rect(data = NULL, aes(xmin=0.0,xmax=0.1,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008) +
#         geom_rect(data = NULL, aes(xmin=0.0,xmax=0.5,ymin=0.3,ymax=0.7), color = "black", fill="#FFFFFF", alpha = 0.0008) +
#         geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008) +
#         annotate(geom = "text", label = "Lipids", x= 0.15, y = 1.75, size = 10, base_family = "georgia")+
#         annotate(geom = "text", label = "Unsaturated \nhydrocarbons", x= 0.05, y = 1.1, size = 5, base_family = "georgia")+
#         annotate(geom = "text", label = "Condensed hydrocarbons", x= 0.25, y = 0.5, size = 10, base_family = "georgia")+
#         annotate(geom = "text", label = "Lignin", x= 0.3, y = 1.1, size = 10, base_family = "georgia")+
        geom_abline(aes(intercept = a, slope = b), data = CHO, size = 1.3) +
        annotate("text", label = CHO$c, x= CHO$x, y= CHO$y, angle = 17.5, size = 6, family = "georgia") +
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0.1, 2)) +
        xlab("O/C atom ratio") +
        ylab("H/C atom ratio") +
        #ggtitle("NO") +
        theme_bw(base_size = 22, base_family = "georgia")
        
p1
png(file = "CHO.png", width = 8, height = 8, units = "in", res = 600)
print(p1)
dev.off()

p2 <- ggplot(df.N.sel, aes(df.N.sel$OC, df.N.sel$HC, colour = NO)) +
        geom_point(size = 3) +
        #scale_color_gradient(low = "cyan", high = "magenta", expression("log10(Abundance)")) +
        #geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=1,ymax=2), fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.3,ymin=1.5,ymax=2), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.1,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.5,ymin=0.3,ymax=0.7), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        #annotate(geom = "text", label = "Lipids", x= 0.15, y = 1.75, size = 10, base_family = "georgia")+
        #annotate(geom = "text", label = "Unsaturated \nhydrocarbons", x= 0.05, y = 1.1, size = 5, base_family = "georgia")+
        #annotate(geom = "text", label = "Condensed hydrocarbons", x= 0.25, y = 0.5, size = 10, base_family = "georgia")+
        #annotate(geom = "text", label = "Lignin", x= 0.3, y = 1.1, size = 10, base_family = "georgia")+
        geom_abline(aes(intercept = a, slope = b), data = CHO, size = 0.75) +
        #annotate("text", label = CHO$c, x= CHO$x, y= CHO$y, angle = 17.5, size = 6, family = "georgia") +
        #geom_text(aes(label = CHO$c, x= Inf, y = CHO$y), hjust = -1)
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0.1, 2)) +
        xlab("O/C atom ratio") +
        ylab("H/C atom ratio") +
        #ggtitle("NO") +
        theme_bw(base_size = 22, base_family = "georgia")
p2

png(file = "NO.png", width = 8, height = 8, units = "in", res = 600)
print(p2)
dev.off()


p3 <- ggplot(df, aes(df$OC, df$HC, colour = "maroon")) +
        geom_point(size = 3) +
        guides(color = FALSE)+
        #scale_color_gradient(low = "cyan", high = "magenta", expression("log10(Abundance)")) +
        #geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=1,ymax=2), fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.3,ymin=1.5,ymax=2), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.1,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.5,ymin=0.3,ymax=0.7), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        #annotate(geom = "text", label = "Lipids", x= 0.15, y = 1.75, size = 10, base_family = "georgia")+
        #annotate(geom = "text", label = "Unsaturated \nhydrocarbons", x= 0.05, y = 1.1, size = 5, base_family = "georgia")+
        #annotate(geom = "text", label = "Condensed hydrocarbons", x= 0.25, y = 0.5, size = 10, base_family = "georgia")+
        #annotate(geom = "text", label = "Lignin", x= 0.3, y = 1.1, size = 10, base_family = "georgia")+
        geom_abline(aes(intercept = a, slope = b), data = CHO, size = 0.75) +
        #annotate("text", label = CHO$c, x= CHO$x, y= CHO$y, angle = 17.5, size = 6, family = "georgia") +
        #geom_text(aes(label = CHO$c, x= Inf, y = CHO$y), hjust = -1)
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0.1, 2)) +
        xlab("O/C atom ratio") +
        ylab("H/C atom ratio") +
        #ggtitle("NO") +
        theme_bw(base_size = 22, base_family = "georgia")

p3

png(file = "van-krevelen-all.png", width = 8, height = 8, units = "in", res = 600)
print(p3)
dev.off()

p4 <- ggplot(df.b, aes(df.b$OC, df.b$HC, colour = log(MW2, base = 2) / log(MW8, base = 2))) +
        geom_point(size = 3) +
        scale_color_gradient(low = "cyan", high = "magenta", expression("log2 Fold change")) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.3,ymin=1.5,ymax=2), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.1,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.0,xmax=0.5,ymin=0.3,ymax=0.7), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008, size = 0.75) +
        #annotate(geom = "text", label = "Lipids", x= 0.15, y = 1.75, size = 10, base_family = "georgia")+
        #annotate(geom = "text", label = "Unsaturated \nhydrocarbons", x= 0.05, y = 1.1, size = 5, base_family = "georgia")+
        #annotate(geom = "text", label = "Condensed hydrocarbons", x= 0.25, y = 0.5, size = 10, base_family = "georgia")+
        #annotate(geom = "text", label = "Lignin", x= 0.3, y = 1.1, size = 10, base_family = "georgia")+
        geom_abline(aes(intercept = a, slope = b), data = CHO, size = 0.75) +
        #annotate("text", label = CHO$c, x= CHO$x, y= CHO$y, angle = 17.5, size = 6, family = "georgia") +
        #geom_text(aes(label = CHO$c, x= Inf, y = CHO$y), hjust = -1)
        scale_x_continuous(limits = c(0, 0.5)) +
        scale_y_continuous(limits = c(0.1, 2)) +
        xlab("O/C atom ratio") +
        ylab("H/C atom ratio") +
        ggtitle("MW2 vs MW8 fold change") +
        theme_bw(base_size = 22, base_family = "georgia")

p4

png(file = "MW2vsMW8fold.png", width = 8, height = 8, units = "in", res = 600)
print(p4)
dev.off()


p4 <- ggplot(df.S, aes(df.S$S, df.S$CHO, colour = log(MW8, base = 10))) +
        geom_point(size = 3) +
        scale_color_gradient(low = "cyan", high = "magenta", expression("log10(Abundance)")) +
        scale_x_continuous(limits = c(0, 5.5)) +
        #scale_y_continuous(limits = c(0.1, 2)) +
        #geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=0.7,ymax=1.5), fill="#FFFFFF", alpha = 0.0008) +
        #geom_rect(data = NULL, aes(xmin=0.1,xmax=0.5,ymin=1,ymax=2), fill="#FFFFFF", alpha = 0.0008) +
        #geom_rect(data = NULL, aes(xmin=0.0,xmax=0.3,ymin=1.5,ymax=2), color = "black", fill="#FFFFFF", alpha = 0.0008) +
        #geom_rect(data = NULL, aes(xmin=0.0,xmax=0.1,ymin=0.7,ymax=1.5), color = "black", fill="#FFFFFF", alpha = 0.0008) +
        #geom_rect(data = NULL, aes(xmin=0.0,xmax=0.5,ymin=0.3,ymax=0.7), color = "black", fill="#FFFFFF", alpha = 0.0008) +
        #geom_abline(aes(intercept = a, slope = b), data = CHO, size = 1.3) +
        #annotate("text", label = CHO$c, x= CHO$x, y= CHO$y, angle = 20, size = 6, family = "georgia") +
        xlab("S") +
        ylab("CHO") +
        ggtitle("MW8") +
        theme_bw(base_size = 22, base_family = "georgia")

p4


grid.arrange(p1,  p3)


