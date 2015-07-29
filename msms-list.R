library(dplyr)

#read phenodata
PhD <- data.frame(read.csv("PhD.csv"))
fNames <- as.character(levels(PhD$X))
fSamples <- as.character(PhD$class)
#positive ionization mode
dataP <- read.csv("positive_featurelist.csv")
dataP$ionization <- "positive"
iso.dataP <- filter(dataP, isotopes != "")
baseP <- filter(iso.dataP, grepl("\\[M\\]\\+", isotopes))
write.csv(baseP, "positive_base_in_feature_extract.csv")

#negative ionization mode
dataN <- read.csv("negative_featurelist.csv")
dataN$ionization <- "negative"
iso.dataN <- filter(dataN, isotopes != "")
baseN <- filter(iso.dataN, grepl("\\[M\\]\\-", isotopes))
write.csv(baseN, "positive_base_in_feature_extract.csv")

#merge lists
dataAll <- bind_rows(dataP, dataN)
write.csv(dataAll, "all_features.csv")
baseAll <- bind_rows(baseP, baseN)
write.csv(baseAll, "base_peaks.csv")
tAll <- data.frame(t(dataAll))
write.csv(tAll, "transpose_all_features.csv")
tBase <- data.frame(t(baseAll))
write.csv(tBase, "transpose_base_peaks.csv")

#lists for plots
dataAll_PCA <- dataAll[ , fNames]
dataAll_PCA <- data.frame(t(dataAll_PCA))
baseAll_PCA <- baseAll[ , fNames]
baseAll_PCA <- data.frame(t(baseAll_PCA))
Samples <- fSamples
        
#S containing compounds
##positive
iso.dataP$peaknum <- gsub(".*\\[([0-9]+).*", "\\1", iso.dataP$isotopes)
SCompP <- select(iso.dataP, isotopes, MW8a)
SCompP$peaknum <- gsub(".*\\[([0-9]+).*", "\\1", SCompP$isotopes)
SbaseP<- filter(SCompP, grepl("\\[M\\]\\+", isotopes))
Splus2P <- filter(SCompP, grepl("\\[M\\+2\\]\\+", isotopes))
SxP <- merge(Splus2P, SbaseP, by = "peaknum")
SxP$ratio <- (SxP$MW8a.x/SxP$MW8a.y)*100
S1CompP <- filter(SxP, 3.5 < ratio, ratio < 7)
S2CompP <- filter(SxP, 7.5 < ratio, ratio < 11.5)
iso.S1P <- filter(iso.dataP, peaknum %in% S1CompP$peaknum)
iso.S2P <- filter(iso.dataP, peaknum %in% S2CompP$peaknum)
iso.SP <- bind_rows(iso.S1P, iso.S2P)
base.SP <- filter(iso.SP, grepl("\\[M\\]\\+", isotopes))


##negative
iso.dataN$peaknum <- gsub(".*\\[([0-9]+).*", "\\1", iso.dataN$isotopes)
SCompN <- select(iso.dataN, isotopes, MW8a)
SCompN$peaknum <- gsub(".*\\[([0-9]+).*", "\\1", SCompN$isotopes)
SbaseN<- filter(SCompN, grepl("\\[M\\]\\-", isotopes))
Splus2N <- filter(SCompN, grepl("\\[M\\+2\\]\\-", isotopes))
SxN <- merge(Splus2N, SbaseN, by = "peaknum")
SxN$ratio <- (SxN$MW8a.x/SxN$MW8a.y)*100
S1CompN <- filter(SxN, 3.5 < ratio, ratio < 7)
S2CompN <- filter(SxN, 7.5 < ratio, ratio < 11.5)
iso.S1N <- filter(iso.dataN, peaknum %in% S1CompN$peaknum)
iso.S2N <- filter(iso.dataN, peaknum %in% S2CompN$peaknum)
iso.SN <- bind_rows(iso.S1N, iso.S2N)
base.SN <- filter(iso.SN, grepl("\\[M\\]\\-", isotopes))

##all S
iso.S <- bind_rows(iso.SP, iso.SN)
base.S <- bind_rows(base.SP, base.SN)
write.csv(iso.S, "S-compounds-iso.csv")
write.csv(base.S, "S-compounds-base.csv")

#prepare S compounds data for PCA
base.S.PCA <- base.S[ , fNames]
base.S.PCA <- data.frame(t(base.S.PCA))


#MSMS list creation
base$log.Art.SW <- log2(base$Art.SW)
base$log.FE1.2 <- log2(base$FE1.2)
base$fold_change <- base$log.FE1.2 / base$log.Art.SW
sel <- filter(base, fold_change >1.2)
sel2 <- filter(sel, FE1.2 > 5000)
write.csv(sel2, "abundant_in_extract_feature.csv")
sel2$rt.min <- sel2$rt/60

msms.list <- select(sel2, mz, rt.min)
write.csv(msms.list, "msms_list.csv")
