library(ggplot2)
library(ggpmisc)
library(RColorBrewer)
library(gridExtra)


source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("tummaddeler_ing_chlorine_dec.csv")
d2 <- reshape2::melt(d1, id = c("Contact.time..hour.", "pH", "Concentration", "Disinfection"))
d3 <- summarySEwithin(d2, measurevar = "value", 
                      withinvars = c("Contact.time..hour.", "pH", "Concentration", "Disinfection", "variable"), idvar = "Contact.time..hour.")
d3[is.na(d3)] <- 0
d3$pH <- factor(d3$pH, levels=c("Unchanged", "10"))

p1 <- ggplot(d3, aes(x = Contact.time..hour., y = value, fill = variable)) +
        geom_bar(position = position_dodge(),stat = "identity") +
        geom_errorbar(position = position_dodge(width = 0.9), 
                      aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
        facet_grid(Concentration~pH, labeller = labeller(pH =label_both))+
        scale_fill_brewer(palette = "Set1", name = "")+
        ylab(expression(paste("C/",C[0], sep = ""))) +
        xlab("Contact time (hour)") +
        theme_bw(base_size =16)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
        ggtitle("Chlorination")

d4 <- read.csv("tummaddeler_ing_chloramine_dec.csv")
d5 <- reshape2::melt(d4, id = c("Contact.time..hour.", "pH", "Concentration", "Disinfection"))
d6 <- summarySEwithin(d5, measurevar = "value", 
                      withinvars = c("Contact.time..hour.", "pH", "Concentration", "Disinfection", "variable"), idvar = "Contact.time..hour.")
d6[is.na(d6)] <- 0
d6$pH <- factor(d6$pH, levels=c("Unchanged", "10"))

p2 <- ggplot(d6, aes(x = Contact.time..hour., y = value, fill = variable)) +
        geom_bar(position = position_dodge(),stat = "identity") +
        geom_errorbar(position = position_dodge(width = 0.9), 
                      aes(ymin = value - sd, ymax = value + sd), width = 0.3)+
        facet_grid(Concentration~pH, labeller = labeller(pH =label_both))+
        scale_fill_brewer(palette = "Set1", name = "")+
        ylab(expression(paste("C/",C[0], sep = ""))) +
        xlab("Contact time (hour)") +
        theme_bw(base_size =16)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")+
        guides(fill = guide_legend(ncol = 2))+
        ggtitle("Chloramination")

grid.arrange(p1, p2, nrow = 2)

dev.print(png, file = "chlorine_chloramine.png", width = 1024, height = 768)
