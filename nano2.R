library(ggplot2)
library(ggpmisc)
library(RColorBrewer)

source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("tummaddeler.csv")
d2 <- reshape2::melt(d1, id = c("Malzeme", "pH", "Sure", "Konsantrasyon"))
d3 <- summarySEwithin(d2, measurevar = "value", withinvars = c("pH", "Sure", "Malzeme", "variable", "Konsantrasyon"), idvar = "Malzeme")

l <- unique(d2$Malzeme)
p1 <- plyr::dlply(if (exists("d3")) {
        .data = d3      
} else {
        .data = d2  
},
"variable", function(x){
        ggplot(x, aes(x = Malzeme, y = value, fill = Konsantrasyon), color = c("red", "green")) +
                geom_bar(position = position_dodge(),stat = "identity") +
                geom_errorbar(position = position_dodge(width = 0.9), aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
                scale_fill_brewer(palette = "Set1", name = "Adsorban \nkonsantrasyonu (mg/L)") +
                facet_grid(Sure~pH, labeller = labeller(pH = label_both))+
                ylab("Konsantrasyon (ng/L)") +
                xlab("Malzeme") +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
                ggtitle(x$variable) 
})
tit <- colnames(d1[5:length(d1)])



x <- length(p1)
for (i in 1:x){
        png(file = paste(tit[[i]], "_nano-atakoy_kis",".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

