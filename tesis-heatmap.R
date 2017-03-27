library(ggplot2)
library(plyr)
source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
d1 <- read.csv("tum_maddeler_ing.csv")
d2 <- summarySEwithin(d1, measurevar = "value", withinvars = c("Numune" ,"variable"), idvar = "Numune")
l2 <- sort(unique(d2$variable))

l <- unique(d1$Numune)

td2 <- transform(d2, Numune = ordered(Numune, l))
td2 <- transform(td2, variable = ordered(variable, l2))

ggplot(td2, aes(x = Numune, y = variable)) + 
        geom_tile(aes(fill = log2(value))) + 
        geom_text(aes(label = round(td2$value, 0))) +
        scale_fill_gradient(low = "green", high = "red") +
        theme_bw(base_size =14) +
        xlab("WWTP Unit") +
        ylab("Compound") +
        theme(legend.position = "none")
