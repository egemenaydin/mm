library(MALDIquant)
library(MALDIquantForeign)
library(rChoiceDialogs)

#Data import
wd <- jchoose.dir(default = getwd(), caption = "Please select directory containing the data")
setwd(wd)

s <- importBrukerFlex(wd)
export(s, path = wd, type = "csv")
tit <- as.list(sapply(s, function(x)metaData(x)$sampleName))

#Quality control
any(sapply(s, isEmpty))
table(sapply(s, length))
any(sapply(s, isRegular))

#Plot mass spectra
for (i in 1:length(s)) {
        png(file = paste(tit[[i]], ".png", sep = ""), width = 16, height = 9, units = "in", res = 600)
        plot(s[[i]])
        dev.off()
}

#Variance stabilization
spectra <- transformIntensity(s, method = "sqrt")

#Smoothing
spectra <- smoothIntensity(spectra, method = "SavitzkyGolay", halfWindowSize = 10)

#Baseline correction
baseline <- estimateBaseline(spectra[[2]], method = "SNIP", iterations = 100)
plot(spectra[[2]])
lines(baseline, col = "red")

spectra <- removeBaseline(spectra, method = "SNIP", iterations = 100)
plot(spectra[[2]])

#Plot baseline corrected mass spectra
for (i in 1:length(spectra)) {
        png(file = paste("BC", tit[[i]], ".png", sep = ""), width = 16, height = 9, units = "in", res = 600)
        plot(spectra[[i]])
        dev.off()
}

#Intensity calibration/Normalization
spectra <- calibrateIntensity(spectra, method = "TIC")

#Wrapping/Alignment
peaks_wr <- detectPeaks(spectra, SNR=5)
reference <- referencePeaks(peaks_wr, minFrequency = 0.5)
spectra <- alignSpectra(spectra, reference = reference)

samples <- factor(sapply(spectra, function(x)metaData(x)$sampleName))
avgSpectra <- averageMassSpectra(spectra, labels = samples, method = "mean")

#Peak detection
noise <- estimateNoise(avgSpectra[[1]])
plot(avgSpectra[[1]], xlim = c(1000, 1050), ylim = c(0, 0.002))
lines(noise, col = "red")
lines(noise[,1], noise[,2]*2, col = "blue")
peaks <- detectPeaks(avgSpectra, method = "MAD", halfWindowSize = 20, SNR = 2)
plot(avgSpectra[[1]], xlim = c(1000, 1050), ylim = c(0, 0.002))
points(peaks[[1]], col = "red", pch = 4)

#Peak binning
peaks <- binPeaks(peaks, tolerance = 0.002)

#Feature matrix
peaks <- filterPeaks(peaks, minFrequency = 0.25)
FM <- intensityMatrix(peaks, avgSpectra)
rownames(FM) <- samples

write.csv(FM, "feature_matrix.csv")
save(list=ls(all=TRUE), file="maldiquant-out.RData")
