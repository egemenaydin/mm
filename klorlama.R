library(ggplot2)
library(ggpmisc)
library(RColorBrewer)


source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("tummaddeler.csv")
d2 <- reshape2::melt(d1, id = c("Sure..saat.", "pH", "Doz"))
d3 <- summarySEwithin(d2, measurevar = "value", 
                      withinvars = c("pH", "Sure..saat.", "variable", "Doz"), idvar = "Sure..saat.")
d3[is.na(d3)] <- 0
d3$pH <- factor(d3$pH, levels=c("Orijinal", "10"))

p1 <- plyr::dlply(if (exists("d3")) {
        .data = d3      
} else {
        .data = d2  
},
"variable", function(x){
        ggplot(x, aes(x = Sure..saat., y = value)) +
                geom_bar(position = position_dodge(),stat = "identity") +
                geom_errorbar(position = position_dodge(), 
                              aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
                facet_grid(Doz~pH, labeller = label_both)+
                scale_fill_brewer(palette = "Set1", name = "")+
                ylab(expression(paste("C/",C[0], sep = ""))) +
                xlab("Süre (saat)") +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(x$variable) 
})
tit <- colnames(d1[4:length(d1)])



x <- length(p1)
for (i in 1:x){
        png(file = paste(tit[[i]], "_kloraminleme",".png", sep = ""))
        print(p1[[i]])
        dev.off()
}