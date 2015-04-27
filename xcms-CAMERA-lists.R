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

slaves <- detectCores(logical = FALSE)


#XCMS for positive mode 

wd <- jchoose.dir(default = getwd(), caption = "Please select directory")

setwd(wd)

xsetP <- xcmsSet(method ="centWave", nSlaves = slaves, ppm = 15, peakwidth = c(10 , 60), snthr = 6, mzdiff = 0.015, prefilter = c(3, 1000))
xset1P <- retcor(xsetP, method = "obiwarp", plottype = c("deviation"), profStep = 0.5)
dev.print(pdf, "RTDvsRT_pos.pdf", height = 10, width = 10)
xset2P <- group(xset1P, bw = 5, minfrac = 0.5, mzwid = 0.015)
xset3P <- fillPeaks(xset2P)
drP <- diffreport(xset3P, class1 = "MW8", class2 = "MW2", "MW2vsMW8", eicmax = 300,  metlin = 0.01)

save(list=ls(all=TRUE), file="air-sparge-pos-xcms-out.RData")

#CAMERA for positive mode

anP <- annotate(xset3P, nSlaves = slaves, perfwhm = 0.75, cor_eic_th = 0.75, minfrac = 0.5, ppm = 5, polarity = "positive", mzabs = 0.015)

#library(chemhelper)
#rulesP <- load.camera.rules("pos")
# an_P <- xsAnnotate(xset3P, nSlaves = slaves)
# an_P <- groupFWHM(an_P, perfwhm = 0.6)
# an_P <- findIsotopes(an_P, mzabs = 0.01)
# an_P <- groupCorr(an_P, cor_eic_th = 0.75)
# anP <- findAdducts(an_P, polarity="positive", rules = rulesP)
peaklistP <- getPeaklist(anP)
write.csv(peaklistP, file = "positive_featurelist.csv")

save(list=ls(all=TRUE), file="air-sparge-pos-camera-out.RData")

cleanParallel(anP)

#positive mode creating peaklists

masses_P <- do.call("rbind", lapply(1:length(anP@pspectra), function(x) {
        neutral_masses_P <- do.call("rbind", lapply(anP@pspectra[[x]], function(y) {
                do.call("rbind", lapply(anP@derivativeIons[[y]], function(z) {
                        cbind(mass = z$mass, rule = z$rule_id, peaknum=y, psg = x)
                }))
        }))
}))


masses_data_P <- as.data.frame(masses_P)

intensity_P <- as.matrix(anP@groupInfo[masses_data_P$peaknum[1:length(masses_data_P$peaknum)], c(4, 16:39)])

P_all <- cbind(masses_data_P, intensity_P)

P_all_ionization <- cbind(P_all, ionization = "positive")

write.csv(P_all_ionization, "positive_all_int.csv")

P_base <- subset(P_all_ionization, P_all_ionization$rule == 1)

write.csv(P_base, "positive_base_int.csv")

save(list=ls(all=TRUE), file="air-sparge-pos-peak-list-out.RData")

#XCMS for negative mode

wd <- jchoose.dir(default = getwd(), caption = "Please select directory")

setwd(wd)

xsetN <- xcmsSet(method ="centWave", nSlaves = slaves, ppm =15, peakwidth = c(10 , 60), snthr = 6, mzdiff = 0.015, prefilter = c(3, 500))
xset1N <- retcor(xsetN, method = "obiwarp", plottype = c("deviation"))
dev.print(pdf, "RTDvsRT_neg.pdf", height = 10, width = 10)
xset2N <- group(xset1N, bw = 5, minfrac = 0.5, mzwid = 0.015)
xset3N <- fillPeaks(xset2N)
drN <- diffreport(xset3N, class1 = "MW8", class2 = "MW2", "MW2vsMW8", eicmax = 300,  metlin = 0.01)

save(list=ls(all=TRUE), file="air-sparge-neg-xcms-out.RData")


#CAMERA for negative mode
anN <- annotate(xset3N, nSlaves = slaves, perfwhm = 0.75, cor_eic_th = 0.75, minfrac = 0.5, ppm = 5, polarity = "negative", mzabs = 0.015)

# rulesN <- load.camera.rules("neg")
# an_N <- xsAnnotate(xset3N, nSlaves = slaves)
# an_N <- groupFWHM(an_N, perfwhm = 0.6)
# an_N <- findIsotopes(an_N, mzabs = 0.01)
# an_N <- groupCorr(an_N, cor_eic_th = 0.75)
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

intensity_N <- as.matrix(anN@groupInfo[masses_data_N$peaknum[1:length(masses_data_N$peaknum)], c(4, 16:39)])

N_all <- cbind(masses_data_N, intensity_N)

N_all_ionization <- cbind(N_all, ionization = "negative")

write.csv(N_all_ionization, "negative_all_int.csv")

N_base <- subset(N_all_ionization, N_all_ionization$rule == 1)

write.csv(N_base, "negative_base_int.csv")


#combining positive and negative mode creating peak lists

all_base <- rbind(N_base, P_base)

write.csv(all_base, "all_base_int.csv")

save(list=ls(all=TRUE), file="air-sparge-all-out.RData")
