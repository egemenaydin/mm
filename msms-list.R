library(dplyr)

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
dataAll_PCA <- select(dataAll, MW1a:MW8c)
dataAll_PCA <- data.frame(t(dataAll_PCA))
baseAll_PCA <- select(baseAll, MW1a:MW8c)
baseAll_PCA <- data.frame(t(baseAll_PCA))
Samples <- c("MW1", "MW1", "MW1", "MW2", "MW2", "MW2", "MW3", "MW3", "MW3", "MW4", "MW4", "MW4", "MW5", "MW5", "MW5", "MW6", "MW6", "MW6", "MW7", "MW7", "MW7", "MW8", "MW8", "MW8")


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
