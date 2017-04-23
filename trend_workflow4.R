library(dplyr)
library(ggplot2)

#load data
df.m <- read.csv("positive_featurelist.csv")

#create names
if("name" %in% colnames(df.m)){
        cat("names were defined\n")
} else{
        df.m$name <- paste("M", round(df.m$mz, 4), "T", round(df.m$rt, 4), sep = "")        
}


#read phenodata
PhD <- data.frame(read.csv("PhDP.csv"))
fNames <- as.vector(PhD$X)
fNames <- gsub("-", "\\.", fNames)
fSamples <- as.vector(PhD$class)
fSamples <- gsub("-", "\\.", fSamples)
grs <- unique(fSamples)

#tidy data
##select base peaks
df1 <- filter(df.m, grepl("\\[M\\]\\+", isotopes))
df2 <- filter(df.m, grepl("\\[M\\]2\\+", isotopes))
df <- bind_rows(df1, df2)

##subset peak names and samples
df <- df.m[ , c("name", fNames)]
row.names(df) <- df$name

for (i in 1:length(grs)) {
        df[[paste(grs[[i]])]] <- apply(df[,grepl(grs[[i]], colnames(df))], 1, mean)
}

df$p1_0vs100 <- apply(select(df, AT100_tp96a:AT100_tp96c, AT0_tp0a:AT0_tp96c), 1, function(x) {
        t.test(x[1:2], x[3:5], paired = FALSE)$p.value
} )

df$logp1_0vs100 <- log(df$p1_0vs100, base = 10)
df$fold_0vs100 <- log2(df$AT0_tp96/df$AT100_tp96)
upr <- filter(df, fold_0vs100 > 2 & logp1_0vs100 < -1)
dor <- filter(df, fold_0vs100 < -2 & logp1_0vs100 < -1)
df.y <- bind_rows(upr, dor)

write.csv(upr, "upreg_AT0vsAT100_tp96.csv")
write.csv(dor, "downreg_AT0vsAT100_tp96.csv")

Csub.D <- df.y[, c("name", fNames)]
row.names(Csub.D) <- Csub.D$name
Csub.D$name <- NULL

##transpose
Csub.D <- data.frame(t(Csub.D))
sample <- fSamples
Csub.D$sample <- fSamples

#tidy more
m.D <- reshape2::melt(Csub.D, id = c("sample"))

#m.D$sample1 <- factor(m.D$sample, as.character(m.D$sample))

#boxplot
p1 <- ggplot(m.D, aes(sample, value)) +
        geom_boxplot() +
        #scale_x_discrete(lables = c("media", "No Biocide", "Low Concentration", "High Concentration", "Blank"))
        xlab("Samples") +
        ylab("Peak area") +
        #ggtitle(m.D$variable) +
        theme_bw(base_size =16) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- plyr::dlply(m.D, "variable", '%+%', e1 = p1)

setwd(paste(getwd(), "/boxplots", sep = ""))

tit <- df.y$name

x <- length(p2)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""), width = 8, height = 8, units = "in", res = 600)
        print(p2[[i]])
        dev.off()
}
DR <- data.frame(names(p2))
write.csv(df.y, "SDB3_high.csv")
colnames(DR) <- "name"
write.csv(DR, "downregulated_name.csv")
MW2 <- filter(df.m, df.m$name %in% tit)
write.csv(MW2, "MW2high.csv")
##upregulated

df.U <- filter(df, HC > 5*media) 
df.U <- filter(df.U, NoBiocide < HC)
df.U <- filter(df.U, biocSt < 5000)
df.U <- filter(df.U, logp1 < -3 | logp2 < -3)
Csub2 <- select(df.U, name:Pooled.c)
row.names(Csub2) <- Csub2$name
Csub2$name <- NULL

##transpose
Csub2 <- data.frame(t(Csub2))
#sample <- c("media", "media", "media", "media", "media", "media", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "Pooled", "Pooled", "Pooled", "Blank", "Blank", "Blank")
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

setwd("~/GitHub/mm/up/")

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

DR.C <- merge(df.m, df.D, by = "name")
UR.C <- merge(df.m, df.y, by = "name")

write.csv(DR.C, "increase_all.csv")
write.csv(UR.C, "increase_onlySDB_high.csv")

#MSMS list creation

msms.list.D <- select(DR.C, mz, rt)
msms.list.U <- select(UR.C, mz, rt)
write.csv(msms.list.D, "msms_downregulated.csv")
write.csv(msms.list.U, "msms_upregulated.csv")

