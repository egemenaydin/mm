library(ggplot2)
library(ggpmisc)
library(RColorBrewer)

source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("tummaddeler.csv")
d2 <- reshape2::melt(d1, id = c("Doz", "pH"))
d3 <- summarySEwithin(d2, measurevar = "value", withinvars = c("pH", "Doz", "variable"), idvar = "Doz")

p1 <- plyr::dlply(if (exists("d3")) {
        .data = d3      
} else {
        .data = d2  
},
"variable", function(x){
        ggplot(x, aes(x = Doz, y = value)) +
                geom_bar(position = position_dodge(),stat = "identity") +
                geom_errorbar(position = position_dodge(), 
                              aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
                facet_grid(~pH, labeller = labeller(pH = label_both))+
                ylab("C/C0") +
                xlab(expression(paste("Doz (mg ", O[3], "/mg ÇOK)"))) +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(x$variable) 
})
tit <- colnames(d1[3:length(d1)])



x <- length(p1)
for (i in 1:x){
        png(file = paste(tit[[i]], "_ozonlama_tuzla_sonbahar",".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

