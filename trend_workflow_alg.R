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
df <- rbind(df1, df2)

##subset peak names and samples
df <- df[ , c("name", fNames)]
row.names(df) <- df$name

for (i in 1:length(grs)) {
        df[[paste(grs[[i]])]] <- apply(df[,grepl(grs[[i]], colnames(df))], 1, mean)
}

df$p1 <- apply(select(df, Mic4a:Mic4c, PCC7806a:PCC7806c), 1, function(x) {
        t.test(x[1:3], x[4:6], paired = FALSE)$p.value
} )

df$logp1 <- log(df$p1, base = 10)

df.D <- filter(df, PCC7806 > 10*Blank)

df.D <- filter(df.D, logp1 < -2)

write.csv(df.D, "PCC-Mic4-diff.csv")

Csub.D <- df.D[, c("name", fNames)]
row.names(Csub.D) <- Csub.D$name
Csub.D$name <- NULL

##transpose
Csub.D <- data.frame(t(Csub.D))
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

tit <- df.D$name

x <- length(p2)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""), width = 4, height = 4, units = "in", res = 600)
        print(p2[[i]])
        dev.off()
}
DR <- data.frame(names(p2))

colnames(DR) <- "name"
write.csv(DR, "PCC_name.csv")
PCC7806 <- filter(df.m, df.m$name %in% tit)
write.csv(PCC7806, "PCC7806.csv")

#search in targets
DB <- read.csv("cya.csv")
db.mz <- DB$mz

ppm.limit <- 5

Cya_name <- matrix("", ncol = 2)
for(i in 1:nrow(df.m)) {
        mz <- df.m[i, 2]
        logical <- abs(((mz - db.mz)/db.mz)*10^6) < ppm.limit
        if(sum(logical)) {
                for(j in length(which(logical))) {
                        Cya_name1 <- matrix("", nrow = 1, ncol = 2)
                        Cya_name1[1,1] <- df.m[i, "name"]
                        Cya_name1[1,2] <- as.vector(DB[which(logical), "Name"])[j]
                        Cya_name <- rbind(Cya_name, Cya_name1)
                }
        }
        else {
                Cya_name1 <- matrix("", nrow = 1, ncol = 2)
                Cya_name1[1,1] <- df.m[i, "name"]
                Cya_name1[1,2] <- "no match"
                Cya_name <- rbind(Cya_name, Cya_name1)
        }
}
        
header <- c("name", "compound")
Cya_name <- data.frame(Cya_name[-1,])
colnames(Cya_name) <- header
cyanatox <- filter(Cya_name, compound != "no match")
detected <- filter(df.m, df.m$name %in% cyanatox$name)
detected$compound <- cyanatox$compound
write.csv(detected, "targeted_search.csv")

#boxplots of targets
det.sub <- detected[, c("name", "compound",fNames)]
cpd <-data.frame(det.sub$name, det.sub$compound)
colnames(cpd) <- c("name", "compound")

det.sub$compound <- paste(det.sub$compound, "_", sub(".*T", "", det.sub$name), sep = "")

row.names(det.sub) <- det.sub$compound
nm <- det.sub$compound
det.sub$name <- NULL
det.sub$compound <- NULL


tds <- data.frame(t(det.sub))
#colnames(tds) <- det.sub$compound
#tds <- as.matrix(tds[-1,])
tds$sample <- fSamples

m.D2 <- reshape2::melt(tds, id = c("sample"))


p3 <- plyr::dlply(
        m.D2, "variable", function(x) {
                ggplot(x, aes(sample, value)) +
                        geom_boxplot() +
                        xlab("Sample") +
                        ylab("Peak area") +
                        theme_bw(base_size =12)+
                        theme(legend.position = "none") +
                        ggtitle(x$variable)
        }
)

#tit <- cyanatox$name

x <- length(p3)

for (i in 1:x){
        png(file = paste(nm[[i]], ".png", sep = ""), width = 4, height = 4, 
            units = "in", res = 600)
        print(p3[[i]])
        dev.off()
}

#MSMS list creation

msms.list.D <- select(DR.C, mz, rt)
msms.list.U <- select(UR.C, mz, rt)
write.csv(msms.list.D, "msms_downregulated.csv")
write.csv(msms.list.U, "msms_upregulated.csv")

