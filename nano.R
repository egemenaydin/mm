library(ggplot2)
library(ggpmisc)
library(RColorBrewer)

source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("all.csv")
d2 <- reshape2::melt(d1, id = c("Malzeme"))
d3 <- summarySEwithin(d2, measurevar = "value", withinvars = c("pH", "Süre..dakika.", "variable"), idvar = "Süre..dakika.")

l <- unique(d2$Malzeme)
p1 <- plyr::dlply(if (exists("d3")) {
        .data = d3      
} else {
        .data = d2  
},
"variable", function(x){
        ggplot(x, aes(x = Malzeme, y = value)) +
                geom_bar(position = position_dodge(),stat = "identity") +
                geom_errorbar(position = position_dodge(), aes(ymin = value - value*0.15, ymax = value + value*0.15), width = 0.3)+
                #facet_grid(~pH, labeller = labeller(pH = label_both))+
                ylab("C/C0") +
                xlab("Malzeme") +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(x$variable) 
})
tit <- colnames(d1[2:length(d1)])



x <- length(p1)
for (i in 1:x){
        png(file = paste(tit[[i]], "_nano_sentetik",".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

