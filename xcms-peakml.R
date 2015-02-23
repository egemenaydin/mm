if(!require("xcms")){
        source("http://bioconductor.org/biocLite.R")
        biocLite("xcms", dependencies = TRUE)
        library(xcms)
}

if(!require("rChoiceDialogs")){
        install.packages("rChoiceDialogs", dependencies = TRUE)
        library(rChoiceDialogs)
}

if(!require("parallel")){
        install.packages("parallel", dependencies = TRUE)
        library(parallel)
}

if(!require("rJava")){
        install.packages("rJava", dependencies = TRUE)
}

if(!require("XML")){
        install.packages("XML", dependencies = TRUE)
}

if(!require("snow")){
        install.packages("snow", dependencies = TRUE)
}

if(!require("caTools")){
        install.packages("caTools", dependencies = TRUE)
}

if(!require("bitops")){
        install.packages("bitops", dependencies = TRUE)
}

if(!require("ptw")){
        install.packages("ptw", dependencies = TRUE)
}

if(!require("gplots")){
        install.packages("gplots", dependencies = TRUE)
}

if(!require("tcltk2")){
        install.packages("tcltk2", dependencies = TRUE)
}

if(!require("R.utils")){
        install.packages("R.utils", dependencies = TRUE)
}

if(!require("mzmatch.R")){
        source ("http://puma.ibls.gla.ac.uk/mzmatch.R/install_mzmatch.R")
}

require (mzmatch.R)
mzmatch.init (version.1=FALSE)


slaves <- detectCores()

wd <- jchoose.dir(default = getwd(), caption = "Please select directory containing mzXML files")

setwd(wd)

mzmatch.R.Setup()


xset <- xcmsSet(method ="centWave", 
                nSlaves = slaves, ppm =5, peakwidth = c(10 , 120), snthresh = 3, prefilter = c(3,1000), integrate = 1, fitgauss = FALSE)
xset1 <- retcor(xset, method = "obiwarp", plottype = c("deviation"))
dev.print(pdf, "RTDvsRT.pdf", height = 9, width = 16)
xset2 <- group(xset1, bw = 5, minfrac = 0.5, mzwid = 0.015)
xset3 <- fillPeaks(xset2)

PeakML.xcms.write.SingleMeasurement(xset = xset3,
                                    filename = sampleList$outputfilenames, ionisation = "detect", ppm = 5,
                                    addscans = 0, ApodisationFilter = FALSE, nSlaves = slaves)

mzmatch.ipeak.Combine(sampleList=sampleList, v=T, rtwindow=60, 
                      combination="set", ppm=5, nSlaves = slaves)

mzmatch.ipeak.filter.RSDFilter(sampleList=sampleList, rsd=0.25,
                               v=T, nSlaves = slaves)

INPUTDIR <- "combined_RSD_filtered"
FILESf <- dir (INPUTDIR,full.names=TRUE,pattern="\\.peakml$")
mzmatch.ipeak.Combine(i=paste(FILESf,collapse=","), v=T, rtwindow=60, 
                      o="final_combined.peakml", combination="set", ppm=5)

PeakML.GapFiller(filename = "final_combined.peakml",
                 ionisation = "detect", 
                 outputfile = "final_combined_gapfilled.peakml", ppm = 0, 
                 rtwin = 0, fillAll=TRUE, Rawpath=NULL)

mzmatch.ipeak.sort.RelatedPeaks(i="final_combined_gapfilled.peakml", v=T,
                                o="final_combined_related.peakml", 
                                basepeaks="final_combined_basepeaks.peakml", ppm=5, rtwindow=30)

DBS <- dir(paste(.find.package("mzmatch.R"), "/dbs", sep=""),
           full.names=TRUE)
DBS
DBS <- paste(DBS[c(1,2,3,4,5)],collapse=",")
mzmatch.ipeak.util.Identify(i="final_combined_related.peakml", v=T,
                            o="final_combined_related_identified.peakml", ppm=5, databases=DBS,
                            adducts="M+H,M+ACN+Na,M+Na,M+K,M+ACN+H")