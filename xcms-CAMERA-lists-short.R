#install necessary packages

if(!require("xcms")){
        source("http://bioconductor.org/biocLite.R")
        biocLite("xcms", dependencies = TRUE)
        library(xcms)
}

if(!require("xcms")){
        source("http://bioconductor.org/biocLite.R")
        biocLite("CAMERA", dependencies = TRUE)
        library(CAMERA)
}


if(!require("rChoiceDialogs")){
        install.packages("rChoiceDialogs", dependencies = TRUE)
        library(rChoiceDialogs)
}

if(!require("dplyr")){
        install.packages("dplyr", dependencies = TRUE)
        library(dplyr)
}

slaves <- 4 #I use it as 1 for now because it looks like there is a bug on CAMERA. When 4 coresused all masses under annoID is represented in last pspgrp.


#XCMS for positive mode 


wd_P <- jchoose.dir(default = getwd(), caption = "Please select directory for positive mode")
wd_N <- jchoose.dir(default = getwd(), caption = "Please select directory for negative mode")

setwd(wd_P)

n_samples_P <- length(list.dirs(recursive = FALSE))

xsetP <- xcmsSet(method ="centWave", nSlaves = slaves, ppm = 10, peakwidth = c(2 , 20), snthr = 6, prefilter = c(0, 300), polarity = "positive")
xset1P <- retcor(xsetP, method = "obiwarp", plottype = c("deviation"), profStep = 0.01)
dev.print(pdf, "RTDvsRT_pos.pdf", height = 10, width = 10)
xset2P <- group(xset1P, bw = 2, minfrac = 0.5, mzwid = 0.015)
xset3P <- fillPeaks(xset2P)
#peaktable_P <- peakTable(xset3P, filebase = "peaktable")
#annotateDiffreport(xset3P, nSlaves = slaves, perfwhm = 0.6, cor_eic_th = 0.75, calcCaS = TRUE, minfrac = 0.5, ppm = 5, polarity = "positive", mzabs = 0.015)
#diffreport(xset3P, "SDB_tp0", "SDB_tp5", eicmax = 500, filebase = "SDB_tp0vstp5")
write.csv(xsetP@phenoData, "PhDP.csv")

save(list=ls(all=TRUE), file="pos-xcms-out.RData")

#CAMERA for positive mode
#library(chemhelper)
#rulesP <- load.camera.rules("pos")
#anP <- annotate(xset3P, nSlaves = slaves, perfwhm = 0.6, cor_eic_th = 0.75, calcCaS = TRUE, minfrac = 0.5, ppm = 5, polarity = "positive", mzabs = 0.015)

library(chemhelper)
rulesP <- load.camera.rules("pos")
anP1 <- xsAnnotate(xset3P, nSlaves = slaves, polarity = "positive")
anP2 <- groupFWHM(anP1, perfwhm = 0.6, sigma = 2)
anP3 <- findIsotopes(anP2, ppm = 5, minfrac = 0.05)
anP4 <- groupCorr(anP3, cor_eic_th = 0.75)
anP5 <- findAdducts(anP4, polarity="positive", rules = rulesP)
cleanParallel(anP5)
peaklistP2 <- getPeaklist(anP5)
isotopes <- getIsotopeCluster(anP5)
write.csv(peaklistP2, file = "positive_featurelist.csv")



library(Rdisop)
mols <- lapply(isotopes, function(x) decomposeIsotopes(x$peaks[,1]-1.007276, x$peaks[,2], z = x$charge, minElements = "C2"))

mol.mat <- matrix(c(1:length(mols),
                    lapply(mols, function(x) x$formula[1]), 
                    lapply(mols, (function(x) x$score[1])), 
                    lapply(mols, function(x) x$exactmass[1]), 
                    lapply(mols, function(x) x$DBE[1]),
                    lapply(mols, function(x) x$charge[1])),  
                  ncol = 6
                  )

colnames(mol.mat) <- c("iso.no", "formula", "score", "exactmass", "DBE", "charge")
mol.mat[mol.mat == "NULL"] <- 0
mol.mat <- data.frame(mol.mat)

mol.mat$iso.no <- unlist(mol.mat$iso.no)
mol.mat$formula <- unlist(mol.mat$formula)
mol.mat$score <- unlist(mol.mat$score)
mol.mat$exactmass <- unlist(mol.mat$exactmass)
mol.mat$DBE <- unlist(mol.mat$DBE)
mol.mat$charge<- unlist(mol.mat$charge)

mol.mat <- filter(mol.mat, charge == 1)

mol.mat <- filter(mol.mat, score > 0.75)
mol.mat <- filter(mol.mat, DBE > 0)
mol.mat$mz.teo <- as.numeric(mol.mat$exactmass) + 1.007276

write.csv(mol.mat, "formula.csv")


save(list=ls(all=TRUE), file="pos-camera-out.RData")


##not used anymore
#positive mode creating peaklists

# masses_P <- do.call("rbind", lapply(1:length(anP@pspectra), function(x) {
#         neutral_masses_P <- do.call("rbind", lapply(anP@pspectra[[x]], function(y) {
#                 do.call("rbind", lapply(anP@derivativeIons[[y]], function(z) {
#                         cbind(mass = z$mass, rule = z$rule_id, peaknum=y, psg = x)
#                 }))
#         }))
# }))
# 
# 
# masses_data_P <- as.data.frame(masses_P)
# 
# intensity_P <- as.matrix(anP@groupInfo[masses_data_P$peaknum[1:length(masses_data_P$peaknum)], c(1, 4, (8+n_samples_P):ncol(anP@groupInfo))])
# 
# P_all <- cbind(masses_data_P, intensity_P)
# 
# P_all_ionization <- cbind(P_all, ionization = "positive")
# 
# P_all_ionization <- distinct(P_all_ionization, mass)
# 
# write.csv(P_all_ionization, "positive_all_int.csv")
# 
# P_base <- subset(P_all_ionization, P_all_ionization$rule == 1)
# 
# write.csv(P_base, "positive_base_int.csv")
# 
# save(list=ls(all=TRUE), file="air-sparge-pos-peak-list-out.RData")

#XCMS for negative mode

setwd(wd_N)

n_samples_N <- length(list.dirs(recursive = FALSE))

xsetN <- xcmsSet(method ="centWave", nSlaves = slaves, ppm =10, peakwidth = c(10 , 100), snthr = 6, mzdiff = 0.01, prefilter = c(3, 100), polarity = "negative")
xset1N <- retcor(xsetN, method = "obiwarp", plottype = c("deviation"), profStep = 0.5)
dev.print(pdf, "RTDvsRT_neg.pdf", height = 10, width = 10)
xset2N <- group(xset1N, bw = 5, minfrac = 0.5, mzwid = 0.015)
xset3N <- fillPeaks(xset2N)
#peaktable_N <- peakTable(xset3N, filebase = "peaktable")
annotateDiffreport(xset3P, nSlaves = slaves, perfwhm = 0.6, cor_eic_th = 0.75, calcCaS = TRUE, minfrac = 0.5, ppm = 5, polarity = "negative", mzabs = 0.015)
write.csv(xsetN@phenoData, "PhDN.csv")

save(list=ls(all=TRUE), file="air-sparge-neg-xcms-out.RData")


#CAMERA for negative mode
#library(chemhelper)
#rulesN <- load.camera.rules("neg")
#anN <- annotate(xset3N, nSlaves = slaves, perfwhm = 0.6, cor_eic_th = 0.75, calcCaS = TRUE, minfrac = 0.5, ppm = 5, polarity = "negative", mzabs = 0.015, rules = rulesN)


# an_N <- xsAnnotate(xset3N, nSlaves = slaves)
# an_N <- groupFWHM(an_N, perfwhm = 0.6)
# an_N <- findIsotopes(an_N, mzabs = 0.01)
# an_N <- groupCorr(an_N, cor_eic_th = 0.75, calcCaS = TRUE)
# anN <- findAdducts(an_N, polarity="negative", rules = rulesN)
peaklistN <- getPeaklist(anN)
write.csv(peaklistN, file = "negative_featurelist.csv")

save(list=ls(all=TRUE), file="air-sparge-neg-camera-out.RData")

cleanParallel(anN)

#negative mode creating peak lists

masses_N <- do.call("rbind", lapply(1:length(anN@pspectra), function(x) {
        neutral_masses_N <- do.call("rbind", lapply(anN@pspectra[[x]], function(y) {
                do.call("rbind", lapply(anN@derivativeIons[[y]], function(z) {
                        cbind(mass = z$mass, rule = z$rule_id, peaknum=y, psg = x)
                }))
        }))
}))


masses_data_N <- as.data.frame(masses_N)

intensity_N <- as.matrix(anN@groupInfo[masses_data_N$peaknum[1:length(masses_data_N$peaknum)], c(1, 4, (8+n_samples_N):ncol(anN@groupInfo))])

N_all <- cbind(masses_data_N, intensity_N)

N_all_ionization <- cbind(N_all, ionization = "negative")

N_all_ionization <- distinct(N_all_ionization, mass)

write.csv(N_all_ionization, "negative_all_int.csv")

N_base <- subset(N_all_ionization, N_all_ionization$rule == 1)

write.csv(N_base, "negative_base_int.csv")


#combining positive and negative mode creating peak lists

all_base <- rbind(N_base, P_base)

all <- rbind(N_all_ionization, P_all_ionization)

write.csv(all_base, "all_base_int.csv")

write.csv(all, "all_int.csv")

peaklistN$ionization <- "negative"

peaklistN$mass <- peaklistN$mz+1.007276

peaklistP$ionization <- "positive"

peaklistP$mass <- peaklistP$mz-1.007276

all_peaklist <- rbind(peaklistN, peaklistP)

all_peaklist <- select(all_peaklist, mz, mass, rt, MW1a:ionization)

all_peaklist <- distinct(all_peaklist, mass)

write.csv(all_peaklist, "all_peaklist.csv")

save(list=ls(all=TRUE), file="air-sparge-all-out.RData")
