library(ggplot2)
library(plyr)
source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
d1 <- read.csv("tesis_tuzla_sb.csv")
d2 <- reshape2::melt(d1, id = c("Numune"))
#when there are replicates use this for summary statistics
d3 <- summarySEwithin(d2, measurevar = "value", withinvars = c("Numune" ,"variable"), idvar = "Numune")

l <- unique(d1$Numune)

p1 <- plyr::dlply(d3, "variable", function (x){
        x <- transform(x, Numune = ordered(Numune, l))
        ggplot(x, aes(x = Numune, y= value, fill = Numune)) +
                geom_bar(position = position_dodge(), stat = "identity") +
                geom_errorbar(position = position_dodge(), 
                              aes(ymin = value - sd, ymax = value + sd), width = 0.3) +
                scale_fill_brewer(palette = "Set1") +
                ylab("Konsantrasyon (ng/L)") +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(x$variable) 
})


x <- length(p1)
tit <- colnames(d1[2:length(d1)])

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

