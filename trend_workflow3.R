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
df <- filter(df.m, grepl("\\[M\\]\\+", isotopes))
df <- read.csv("increase_high_SDB2.csv")
##subset peak names and samples
df <- df.m[ , c("name", fNames)]
row.names(df) <- df$name

for (i in 1:length(grs)) {
        df[[paste(grs[[i]])]] <- apply(df[,grepl(grs[[i]], colnames(df))], 1, mean)
}

df$p1 <- apply(select(df, SDB_tp3a:SDB_tp3d, SDB_tp5a:SDB_tp5f), 1, function(x) {
        t.test(x[1:2], x[3:8], paired = FALSE)$p.value
} )

df$logp1 <- log(df$p1, base = 10)

df.D <- filter(df, C6 > 10*Blank)
df.C6 <- filter(df.D, C6 > 5*sterile)
df.C6.2 <- filter(df.C6, C6 > 2*C4)
df.C4C6 <- filter(df.D, C4 > 5*sterile & C6 > 5* sterile)

df.D <- filter(df.D, logp1 < -2)
df.D <- filter(df.D, SDB_tp5 > 10*blank)
write.csv(df.C4C6, "C4C6high.csv")
df.x <- filter(df.D, SDB_tp5 > C_tp5)
df.y <- filter(df, SDB_tp3 > 5*C_tp3)
df.y <- filter(df.y, SDB_tp3 > 10 * blank)
df.y <- filter(df.y, SDB_tp3 > SDB_tp5)
df.y <- filter(df.y, logp1 < -2)

write.csv(df.x, "increase_high_SDB2.csv")
write.csv(df.y, "increase_high_SDB3.csv")

df$p1 <- apply(select(df, MW2a:MW2c, MW8a:MW8c), 1, function(x) {
        t.test(x[1:3], x[4:6], paired = FALSE)$p.value
} )

df$logp1 <- log(df$p1, base = 10)

df$fc <- log(df$MW2/df$MW8, base = 2)

MW2High <- filter(df, fc > 1)
MW2High <- filter(MW2High, MW2 > 10 * Blank)
MW2Low <- filter(df.D, MW2 < quantile(MW2, probs = 0.10))


MW8High <- filter(df.D, MW8 > quantile(MW8, probs = 0.90))
MW8Low <- filter(df.D, MW8 < quantile(MW8, probs = 0.10))



Csub.D <- df.y[, c("name", fNames)]
row.names(Csub.D) <- Csub.D$name
Csub.D$name <- NULL

##transpose
Csub.D <- data.frame(t(Csub.D))
sample <- fSamples
        c("Acid Standard", "Acid Standard", "Acid Standard", "Biocide Standard", "Biocide Standard", "Biocide Standard", "Blank", "Blank", "Blank", "Blank","No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "No Biocide", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "High Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "Low Concentration", "media", "media", "media", "media", "media", "media", "Pooled", "Pooled", "Pooled")
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

setwd(paste(getwd(), "/SDB3", sep = ""))

tit <- df.y$name

x <- length(p2)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""), width = 4, height = 4, units = "in", res = 600)
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

