library(ggplot2)
library(plyr)
source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
d1 <- read.csv("tum_maddeler_tuzla_long_ing.csv")
d2 <- summarySEwithin(dplyr::filter(d1, !is.na(value)), measurevar = "value", 
                      withinvars = c("Numune" ,"variable", "Season"), idvar = "Numune")
d3 <- dplyr::filter(d2, variable == "Naproxen" | 
                            variable == "Hydroxydiclofenac" | 
                            variable == "Carbamazepine" | 
                            variable == "Sulfamethoxazole" | 
                            variable == "Epoxycarbamazepine"|
                            variable == "Diclofenac"|
                            variable == "Dihydrohydroxycarbamazepine")
l2 <- sort(unique(d2$variable))

l <- unique(d1$Numune)

td2 <- transform(d3, Numune = ordered(Numune, l))
td2 <- transform(td2, variable = ordered(variable, l2))

td2$Season <- factor(td2$Season, levels = c("Summer", "Fall", "Winter"))

ggplot(td2, aes(x = Numune, y = variable)) + 
        geom_tile(aes(fill = log2(value))) + 
        #geom_text(aes(label = round(td2$value, 0))) +
        facet_wrap(~Season, ncol=3) +
        scale_fill_gradient(low = "green", high = "red") +
        theme_minimal(base_size =24) +
        xlab("WWTP Unit") +
        ylab("Compound") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(legend.position = "none")

dev.print(png, "heatmap_tuzla_select.png", height = 9, width = 16, res = 600, unit = "in")
