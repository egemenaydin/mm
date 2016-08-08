library(ggplot2)
library(plyr)
source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
d1 <- read.csv("nano-sentetik-all.csv")
d2 <- reshape2::melt(d1, id = c("Malzeme", "pH", "Süre"))
#when there are replicates use this for summary statistics
d3 <- summarySEwithin(d2, measurevar = "value", withinvars = c("pH", "Süre", "Malzeme", "variable"), idvar = "Malzeme")

p1 <- plyr::dlply(if (exists("d3")) {
        .data = d3      
  } else {
          .data = d2  
  },
        "variable", function(x){
        ggplot(x, aes(x = Malzeme, y = value, fill = Süre)) +
                geom_bar(position = position_dodge(),stat = "identity") +
                geom_errorbar(position = position_dodge(), aes(ymin = value - sd, ymax = value + sd))+
                facet_grid(~pH, labeller = labeller(pH = label_both))+
                ylab("C/C0") +
                #ggtitle(p1$data$variable) +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(x$variable) 
})


x <- length(p1)
tit <- colnames(d1[2:21])

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

