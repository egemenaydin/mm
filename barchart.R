library(ggplot2)
library(dplyr)
d1 <- read.csv("nano-sentetik-negatif.csv")
d2 <- reshape2::melt(d1, id = c("Malzeme", "pH", "Süre"))

p1 <- plyr::dlply(d2, .(variable), function(x){
        ggplot(x, aes(x = Malzeme, y = value)) +
                geom_bar(stat = "identity") +
                facet_grid(pH~Süre, labeller = label_both)+
                xlab("Samples") +
                ylab("C/C0") +
                #ggtitle(p1$data$variable) +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(x$variable)
})


x <- length(p1)
tit <- colnames(d1[2:12])

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}
