library(dplyr)
library(ggplot2)


#load data
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

#tidy data
##select base peaks
df <- filter(df.m, grepl("\\[M\\]\\+", isotopes))
df$iso.no <- gsub("[^0-9]", "", df$isotopes)
        
f <- read.csv("formula.csv")
df.b <- merge(f,df, by = "iso.no")
