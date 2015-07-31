library(dplyr)
library(ggplot2)

#load data
df.m <- read.delim("XCMS.annotated.diffreport.tsv")

#tidy data
##select base peaks
df <- filter(df.m, isotopes != "")
df <- filter(df, grepl("\\[M\\]\\+", isotopes))

##subset peak names and samples
df <- select(df, name, M1a:Blank_002)
row.names(df) <- df$name
df$media <- apply(select(df, M1a:M3b), 1, mean)
df$NoBiocide <- apply(select(df, contains("HB")), 1, mean)
df$LC <- apply(select(df, contains("Low")), 1, mean)
df$HC <- apply(select(df, contains("High")), 1, mean)
df$Blank <- apply(select(df, contains("Blank")), 1, mean)
df$p1 <- apply(select(df, HB1a:Low3b), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )
df$logp1 <- log(df$p1, base = 10)

df$p2 <- apply(select(df, HB1a:HB3b, High1a:High3b), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )

df$logp2 <- log(df$p1, base = 10)

df$p3 <- apply(select(df, HB1a:HB3b, Blank_004:Blank_002), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )
df$logp3 <- log(df$p1, base = 10)

df <- filter(df, NoBiocide > LC & NoBiocide > HC & NoBiocide > Blank)
df <- filter(df, logp1 < -2 | logp2 < -2)
Csub <- select(df, name:Blank_002)
row.names(Csub) <- Csub$name
Csub$name <- NULL

##transpose
Csub <- data.frame(t(Csub))
sample <- c("media", "media", "media", "media", "media", "media", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "Pooled", "Pooled", "Pooled", "Blank", "Blank", "Blank")
Csub$sample <- sample

#tidy more
m <- reshape2::melt(Csub, id = c("sample"))
m$sample1 <- factor(m$sample, as.character(m$sample))

#boxplot
p1 <- ggplot(m, aes(sample1, value)) +
        geom_boxplot() +
        #scale_x_discrete(lables = c("media", "No Biocide", "Low Concentration", "High Concentration", "Blank"))
        xlab("Samples") +
        ylab("Intensity (counts)") +
        ggtitle(m$variable) +
        theme_bw(base_size =16) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- plyr::dlply(m, "variable", '%+%', e1 = p1)

setwd("~/GitHub/mm/box2/")

tit <- df$name

x <- length(p2)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p2[[i]])
        dev.off()
}
DR <- data.frame(names(p2))
colnames(DR) <- "name"
write.csv(DR, "downregulated.csv")

##upregulated
#tidy data
##select base peaks
df2 <- filter(df.m, isotopes != "")
df2 <- filter(df2, grepl("\\[M\\]\\+", isotopes))

##subset peak names and samples
df2 <- select(df2, name, M1a:Blank_002)
row.names(df2) <- df2$name
df2$media <- apply(select(df2, M1a:M3b), 1, mean)
df2$NoBiocide <- apply(select(df2, contains("HB")), 1, mean)
df2$LC <- apply(select(df2, contains("Low")), 1, mean)
df2$HC <- apply(select(df2, contains("High")), 1, mean)
df2$Blank <- apply(select(df2, contains("Blank")), 1, mean)
df2$p1 <- apply(select(df2, HB1a:Low3b), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )
df2$logp1 <- log(df2$p1, base = 10)

df2$p2 <- apply(select(df2, HB1a:HB3b, High1a:High3b), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )

df2$logp2 <- log(df2$p1, base = 10)

df2$p3 <- apply(select(df2, HB1a:HB3b, Blank_004:Blank_002), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )
df2$logp3 <- log(df2$p1, base = 10)
df2 <- filter(df2, NoBiocide < LC | NoBiocide < HC)
df2 <- filter(df2, logp1 < -2 | logp2 < -2)
Csub2 <- select(df2, name:Blank_002)
row.names(Csub2) <- Csub2$name
Csub2$name <- NULL

##transpose
Csub2 <- data.frame(t(Csub2))
sample <- c("media", "media", "media", "media", "media", "media", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "Pooled", "Pooled", "Pooled", "Blank", "Blank", "Blank")
Csub2$sample <- sample

#tidy more
m2 <- reshape2::melt(Csub2, id = c("sample"))
m2$sample1 <- factor(m2$sample, as.character(m2$sample))

#boxplot
p3 <- ggplot(m2, aes(sample1, value)) +
        geom_boxplot() +
        #scale_x_discrete(lables = c("media", "No Biocide", "Low Concentration", "High Concentration", "Blank"))
        xlab("Samples") +
        ylab("Intensity (counts)") +
        ggtitle(m2$variable) +
        theme_bw(base_size =16) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- plyr::dlply(m2, "variable", '%+%', e1 = p3)

setwd("~/GitHub/mm/box1/")

tit2 <- df2$name

x2 <- length(p4)

for (i in 1:x){
        png(file = paste(tit2[[i]], ".png", sep = ""))
        print(p4[[i]])
        dev.off()
}
UR <- data.frame(names(p4))
colnames(UR) <- "name"
write.csv(UR, "upregulated.csv")
