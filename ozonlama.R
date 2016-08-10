library(ggplot2)
library(ggpmisc)
library(RColorBrewer)

source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("ozon-sentetik.csv")
d2 <- reshape2::melt(d1, id = c("Süre..dakika.", "pH"))
d3 <- summarySEwithin(d2, measurevar = "value", withinvars = c("pH", "Süre..dakika.", "variable"), idvar = "Süre..dakika.")

p1 <- plyr::dlply(if (exists("d3")) {
        .data = d3      
} else {
        .data = d2  
},
"variable", function(x){
        ggplot(x, aes(x = Süre..dakika., y = value)) +
                geom_bar(position = position_dodge(),stat = "identity") +
                geom_errorbar(position = position_dodge(), aes(ymin = value - sd, ymax = value + sd))+
                facet_grid(~pH, labeller = labeller(pH = label_both))+
                ylab("C/C0") +
                xlab("Süre (dakika)") +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(x$variable) 
})
tit <- colnames(d1[2:11])



x <- length(p1)
for (i in 1:x){
        png(file = paste(tit[[i]], "_ozonlama",".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

