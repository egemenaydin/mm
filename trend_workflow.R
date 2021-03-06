library(dplyr)
library(ggplot2)

#load data
df.m <- read.csv("positive_featurelist.csv")

#read phenodata
PhD <- data.frame(read.csv("PhD.csv"))
fNames <- as.character(levels(PhD$X))
fNames <- gsub("-", "\\.", fNames)
fSamples <- as.character(PhD$class)
fSamples <- gsub("-", "\\.", fSamples)
grs <- unique(fSamples)

#tidy data
##select base peaks
df <- filter(df.m, grepl("\\[M\\]\\+", isotopes))

##subset peak names and samples
df <- df[ , c("name", fNames)]
row.names(df) <- df$name

for (i in 1:length(grs)) {
        df[[paste(grs[[i]])]] <- apply(df[,grepl(grs[[i]], colnames(df))], 1, mean)
}

df$p1 <- apply(select(df, HB1a:Low3b), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )
df$logp1 <- log(df$p1, base = 10)

df$p2 <- apply(select(df, HB1a:HB3b, High1a:High3b), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )

df$logp2 <- log(df$p2, base = 10)

df$p3 <- apply(select(df, HB1a:HB3b, Blank_004:Blank_002), 1, function(x) {
        t.test(x[1:6], x[7:12], paired = FALSE)$p.value
} )
df$logp3 <- log(df$p3, base = 10)

df.D <- filter(df, NoBiocide > LC & NoBiocide > HC & NoBiocide > Blank & NoBiocide > 10*media)
df.D <- filter(df.D, logp1 < -3 & logp2 < -3)
Csub.D <- select(df.D, name:Blank_002)
row.names(Csub.D) <- Csub.D$name
Csub.D$name <- NULL

##transpose
Csub.D <- data.frame(t(Csub.D))
sample <- c("media", "media", "media", "media", "media", "media", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "Pooled", "Pooled", "Pooled", "Blank", "Blank", "Blank")
Csub.D$sample <- sample

#tidy more
m.D <- reshape2::melt(Csub.D, id = c("sample"))
m.D$sample1 <- factor(m.D$sample, as.character(m.D$sample))

#boxplot
p1 <- ggplot(m.D, aes(sample1, value)) +
        geom_boxplot() +
        #scale_x_discrete(lables = c("media", "No Biocide", "Low Concentration", "High Concentration", "Blank"))
        xlab("Samples") +
        ylab("Intensity (counts)") +
        ggtitle(m.D$variable) +
        theme_bw(base_size =16) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- plyr::dlply(m.D, "variable", '%+%', e1 = p1)

setwd("~/mm/down/")

tit <- df.D$name

x <- length(p2)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p2[[i]])
        dev.off()
}
DR <- data.frame(names(p2))
colnames(DR) <- "name"
write.csv(DR, "downregulated_name.csv")

##upregulated

df.U <- filter(df, HC > 5*media) 
df.U <- filter(df.U, NoBiocide < HC)
df.U <- filter(df.U, logp1 < -3 | logp2 < -3)
Csub2 <- select(df.U, name:Blank_002)
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
        #ggtitle(m2$variable) +
        theme_bw(base_size =16) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- plyr::dlply(m2, "variable", '%+%', e1 = p3)

setwd("~/mm/up/")

tit2 <- df.U$name

x2 <- length(p4)

for (j in 1:x2){
        png(file = paste(tit2[[j]], ".png", sep = ""))
        print(p4[[j]])
        dev.off()
}
UR <- data.frame(names(p4))
colnames(UR) <- "name"
write.csv(UR, "upregulated_name.csv")

#subset upregulated and downregulated compounds

DR.C <- merge(df.m, DR, by = "name")
UR.C <- merge(df.m, UR, by = "name")

write.csv(DR.C, "downregulated_compounds.csv")
write.csv(UR.C, "upregulated_compounds.csv")

#MSMS list creation

msms.list.D <- select(DR.C, mzmed, rtmed)
msms.list.U <- select(UR.C, mzmed, rtmed)
write.csv(msms.list.D, "msms_downregulated.csv")
write.csv(msms.list.U, "msms_upregulated.csv")
