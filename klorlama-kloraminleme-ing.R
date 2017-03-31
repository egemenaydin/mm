library(ggplot2)
library(ggpmisc)
library(RColorBrewer)


source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("tummadeler_ing.csv")
d1$ContactTime <- as.factor(d1$ContactTime)
d1$pH <- as.factor(d1$pH)
d1$Dosage <- as.factor(d1$Dosage)
d2 <- reshape2::melt(d1, id = c("ContactTime", "pH", "Dosage", "Disinfection"))
d3 <- summarySEwithin(d2, measurevar = "value", 
                      withinvars = c("pH", "ContactTime", "variable", "Dosage", 
                                     "Disinfection"), idvar = "ContactTime")
d3[is.na(d3)] <- 0
d3$pH <- factor(d3$pH, levels=c("Unchanged", "10"))

p1 <- plyr::dlply(if (exists("d3")) {
        .data = d3      
} else {
        .data = d2  
},
"variable", function(x){
        ggplot(x, aes(x = ContactTime, y = value, fill = Disinfection)) +
                geom_bar(position = position_dodge(),stat = "identity") +
                geom_errorbar(position = position_dodge(width = 0.9), 
                              aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
                facet_grid(Dosage~pH, labeller = label_both)+
                scale_fill_brewer(palette = "Set1", name = "")+
                ylab(expression(paste("C/",C[0], sep = ""))) +
                xlab("Time (hour)") +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
                ggtitle(x$variable) 
})
tit <- colnames(d1[5:length(d1)])



x <- length(p1)
for (i in 1:x){
        png(file = paste(tit[[i]], "_Disinfection",".png", sep = ""))
        print(p1[[i]])
        dev.off()
}
