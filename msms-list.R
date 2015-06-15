library(dplyr)
data <- read.csv("positive_featurelist.csv")
iso.data <- filter(data, isotopes != "")
base <- filter(iso.data, grepl("\\[M\\]\\+", isotopes))
base$log.Art.SW <- log2(base$Art.SW)
base$log.FE1.2 <- log2(base$FE1.2)
base$fold_change <- base$log.FE1.2 / base$log.Art.SW
sel <- filter(base, fold_change >1.2)
sel2 <- filter(sel, FE1.2 > 5000)
write.csv(sel2, "abundant_in_extract_feature.csv")
sel2$rt.min <- sel2$rt/60

msms.list <- select(sel2, mz, rt.min)
write.csv(msms.list, "msms_list.csv")
