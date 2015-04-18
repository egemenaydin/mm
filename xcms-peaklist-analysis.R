#negative_PL <- as.data.frame(read.csv("negative_peaklist.csv"))
#negative_IPL <- as.data.frame(subset(negative_PL, negative_PL$isotopes != ""))

#for positive mode only
# no_frag <- pspec2metfrag(anP)
# 
# no_fragment <- data.frame(mass = 0, rt = 0, ID = 0)
# 
# for (i in 1:length(no_frag)){
#         no_fragment[i,1] <- no_frag[[i]]$ParentMass
#         no_fragment[i,2] <- no_frag[[i]]$RentionTime
#         no_fragment[i,3] <- no_frag[[i]]$AnnotationID
# }

#write.csv(no_fragment, "positive_base_peaks.csv")

#negative
masses_N <- do.call("rbind", lapply(1:length(anN@pspectra), function(x) {
        neutral_masses_N <- do.call("rbind", lapply(anN@pspectra[[x]], function(y) {
                do.call("rbind", lapply(anN@derivativeIons[[y]], function(z) {
                        cbind(neutral = z$mass, rule = z$rule_id, peaknum=y, psg = x)
                }))
        }))
}))


masses_data_N <- as.data.frame(masses_N)

intensity_N <- as.matrix(anN@groupInfo[masses_data_N$peaknum[1:length(masses_data_N$peaknum)], c(4, 16:39)])

N_all <- cbind(masses_data_N, intensity_N)

N_all_ionization <- cbind(N_all, ionization = "negative")

write.csv(N_all_ionization, "negative_all_int.csv")

N_base <- subset(N_all_ionization, y$rule == 1)

write.csv(N_base, "negative_base_int.csv")


#positive
masses_P <- do.call("rbind", lapply(1:length(anP@pspectra), function(x) {
        neutral_masses_P <- do.call("rbind", lapply(anP@pspectra[[x]], function(y) {
                do.call("rbind", lapply(anP@derivativeIons[[y]], function(z) {
                        cbind(neutral = z$mass, rule = z$rule_id, peaknum=y, psg = x)
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

all_base <- rbind(N_base, P_base)

#selecting ions more than one adduct
# withinppm <- function(m1, m2, ppm=5) {   abs(m1-m2)/m1*1E6 < 5   }
# 
# h_pairs <- do.call("rbind",lapply(which(y[,"rule"] == 1), function(x) {
#         mh <- y[x,,drop=F]
#         a_pair <- y[
#                 y[,"psg"] == mh[,"psg"] &
#                         withinppm(y[,"neutral"], mh[,"neutral"]) &
#                         y[,"rule"] == 1,
#                 
#                 ,drop=F]
#         
#         if(nrow(a_pair) < 1) {return(NULL)}
#         
#         cbind(neutral=mh[,"neutral"],psg = mh[,"psg"], mh_peaknum = mh[,"peaknum"], mcl_peaknum = a_pair[,"peaknum"])
#         
# }))
# 
# hcl_pairs <- do.call("rbind",lapply(which(all_masses[,"rule"] == 1), function(x) {
#         mh <- all_masses[x,,drop=F]
#         a_pair <- all_masses[
#                 all_masses[,"psg"] == mh[,"psg"] &
#                         withinppm(all_masses[,"neutral"], mh[,"neutral"]) &
#                         all_masses[,"rule"] == 7,
#                 
#                 ,drop=F]
#         
#         if(nrow(a_pair) < 1) {return(NULL)}
#         
#         cbind(neutral=mh[,"neutral"],psg = mh[,"psg"], mh_peaknum = mh[,"peaknum"], mcl_peaknum = a_pair[,"peaknum"])
#         
# }))

#nontarget
# negative_nontarget <- subset(negative_PL, select = c(mz, MW8a, rt))
# names(negative_nontarget)[names(negative_nontarget) == "MW8a"] <- "intensity"
# iso <- make.isos(iso_list, charges = c(-1), elements = c("C", "O", "H", "N", "S"))


#metaMS
# Agilent.QToF <- metaMSsettings(protocolName = "Agi.QToF",
#                                chrom = "LC",
#                                PeakPicking = list(
#                                        method = "centWave",
#                                        step = 0.05,
#                                        fwhm = 20,
#                                        snthresh = 4,
#                                        max = 50),
#                                Alignment = list(
#                                        min.class.fraction = .3,
#                                        min.class.size = 3,
#                                        mzwid = 0.1,
#                                        bws = c(130, 10),
#                                        missingratio = 0.2,
#                                        extraratio = 0.1,
#                                        retcormethod = "linear",
#                                        retcorfamily = "symmetric",
#                                        fillPeaks = TRUE),
#                                CAMERA = list(
#                                        perfwhm = 0.6,
#                                        cor_eic_th = 0.7,
#                                        ppm= 5))


#MAIT
# source("http://bioconductor.org/biocLite.R")
# biocLite("MAIT", dependencies = T)
# library(MAIT)
# sampleProcessing(dataDir = "neg", project = "air-sparge", peakwidth = c(5,150), ppm = 15, nSlaves = 4)
