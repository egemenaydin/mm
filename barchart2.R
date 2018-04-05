library(ggplot2)
library(plyr)
source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
#d1 <- read.csv("nano-sentetik-all.csv")
d1 <- dplyr::select(caro_bio, Metabolite, name, KCl.y:NaCl_I.y)
d2 <- reshape2::melt(d1, id = c("Metabolite", "name"))
#when there are replicates use this for summary statistics
#d3 <- summarySEwithin(d2, measurevar = "value", withinvars = c("pH", "Süre", "Malzeme", "variable"), idvar = "Malzeme")

p1 <- plyr::dlply(.data=d2, "name", function(x){
        ggplot(x, aes(x = variable, y = value)) +
                geom_bar(position = position_dodge(),stat = "identity") +
                #geom_errorbar(position = position_dodge(), aes(ymin = value - sd, ymax = value + sd))+
                #facet_grid(~pH, labeller = labeller(pH = label_both))+
                ylab("Peak area") +
                #ggtitle(p1$data$variable) +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(x$Metabolite) 
})


x <- length(p1)
tit <- d1$name

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

