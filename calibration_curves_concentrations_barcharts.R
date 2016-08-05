library(ggplot2)
library(ggpmisc)
library(RColorBrewer)

source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
d1 <- read.csv("std.csv")
d2 <- reshape2::melt(d1, id = c("Standard"))
#when there are replicates use this for summary statistics
d3 <- summarySEwithin(d2, measurevar = "value", withinvars = c("pH", "Süre", "Malzeme", "variable"), idvar = "Malzeme")

my.formula <- y ~ x
p1 <- plyr::dlply(if (exists("d3")) {
        .data = d3      
} else {
        .data = d2  
},
        "variable", function(x){
        ggplot(x, aes(x = Standard, y = value)) +
                geom_smooth(method="lm",se=FALSE, color = "black", formula = my.formula) +
                stat_poly_eq(formula = my.formula, 
                             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                             parse = TRUE) +   
                geom_point()+
                xlab("Concentration (ng/L)") +
                ylab("Area/Area") +
                theme_bw(base_size =16)+
                ggtitle(x$variable) 
})


x <- length(p1)
tit <- colnames(d1[2:12])

for (i in 1:x){
        png(file = paste(tit[[i]], "_calibration_curve",".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

models <- plyr::dlply(if (exists("d3")) {
        .data = d3      
} else {
        .data = d2  
},
        "variable", function (x) {
          lm(Standard ~ value, data = x)      
        })

r1 <- read.csv("tesis.csv")
r2 <- reshape2::melt(r1, id = c("Numune"))

c <- plyr::ddply(r2, "variable", function(x) transform(x, value = predict(models[[paste(x$variable[1])]], newdata = x)))

c$concentration <- c$value/2

c_stat <- summarySEwithin(c, measurevar = "concentration", withinvars = c("Numune", "variable"), idvar = "variable")

l <- unique(r1$Numune)

p2 <- plyr::dlply(c_stat, "variable", function (x){
        x <- transform(x, Numune = ordered(Numune, l))
        ggplot(x, aes(x = Numune, y= concentration, fill = Numune)) +
                geom_bar(position = position_dodge(), stat = "identity") +
                geom_errorbar(position = position_dodge(), 
                              aes(ymin = concentration - sd, ymax = concentration + sd), width = 0.3) +
                scale_fill_brewer(palette = "Set1") +
                ylab("Konsantrasyon (ng/L)") +
                theme_bw(base_size =16)+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(x$variable) 
})

y <- length(p2)
tit_compound <- colnames(r1)[-1]

for (i in 1:y){
        png(file = paste(tit_compound[[i]], "_konsantrasyon",".png", sep = ""))
        print(p2[[i]])
        dev.off()
}

