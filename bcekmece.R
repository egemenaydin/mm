library(ggplot2)
library(ggpmisc)
library(RColorBrewer)

source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("bcekmece.csv")

d2 <- reshape2::melt(d1, id = c("Compound", "Month"))
d3 <- dplyr::filter(d2, value != "NA")

p1 <- plyr::dlply(d3, "variable", function (x) {
        ggplot(x, aes(x = Compound, y = value)) +
                #geom_boxplot() +
                geom_point(aes(color = Month), size = 5) +
                labs(x = "Madde", y = "Konsantrasyon (ng/L)") +
                scale_color_brewer(palette = "Set1") +
                scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
                theme_bw(base_size = 26) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                ggtitle(x$variable)
})

tit <- unique(d3$variable)

x <- length(p1)
for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""), height = 9, width = 9, res = 600, unit = "in")
        print(p1[[i]])
        dev.off()
}
 