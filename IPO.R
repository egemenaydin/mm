library(IPO)

wd_P <- jchoose.dir(default = getwd(), caption = "Please select directory for positive mode")
setwd(wd_P)

datafiles <- list.files(wd_P, recursive = TRUE, full.names = TRUE)

peakpickingParameters <- getDefaultXcmsSetStartingParams('centWave')
#setting levels for step to 0.2 and 0.3 (hence 0.25 is the center point)
peakpickingParameters$ppm <- c(2, 20)
peakpickingParameters$mzdiff <- c(-0.001, 0.02)
peakpickingParameters$min_peakwidth <- c(0.5, 20)
peakpickingParameters$max_peakwidth <- c(20, 90)
peakpickingParameters$value_of_prefilter <- c(100, 10000)
peakpickingParameters$prefilter <- c(0,3)

time.xcmsSet <- system.time({ # measuring time
        resultPeakpicking <- 
                optimizeXcmsSet(files = datafiles[19:21], 
                                params = peakpickingParameters, 
                                nSlaves = 1, 
                                subdir = NULL)
})
