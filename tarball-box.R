library(ggplot2)
library(dplyr)
library(ggthemes)

df <- read.csv("aromatic-HC-metabolites-semilong.csv", check.names = F)
names <- read.csv("tarball-samples.csv")

m.D <- reshape2::melt(df)

options(scipen=100000000)

l <- unique(names$Samples)


p1 <- plyr::dlply(m.D, "Compound", function(x) {
        ggplot(transform(x, variable = ordered(variable, l)), aes(variable, value)) +
                geom_boxplot() +
                xlab("Samples") +
                ylab("Peak area") +
                theme_base(base_size =20) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                ggtitle(x$Compound)
})


tit <- 1:length(unique(df$Compound))

tit2 <- m.D$Compound


x <- length(p1)
for (i in 1:x){
        png(file = paste(p1[[i]]$data$Compound[1],".png", sep = ""), res = 600, height = 6, width = 8, units = "in")
        print(p1[[i]])
        dev.off()
}
