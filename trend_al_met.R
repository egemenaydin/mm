library(ggplot2)

at_deg_data <- filter(df.m, mz %in% At_deg$mz)

Csub.D <- at_deg_data[, c("name", fNames)]
row.names(Csub.D) <- Csub.D$name
Csub.D$name <- NULL

##transpose
Csub.D <- data.frame(t(Csub.D))
sample <- fSamples
Csub.D$sample <- fSamples

#tidy more
m.D <- reshape2::melt(Csub.D, id = c("sample"))

#m.D$sample1 <- factor(m.D$sample, as.character(m.D$sample))

#boxplot
p1 <- ggplot(m.D, aes(sample, value)) +
        geom_boxplot() +
        #scale_x_discrete(lables = c("media", "No Biocide", "Low Concentration", "High Concentration", "Blank"))
        xlab("Samples") +
        ylab("Peak area") +
        #ggtitle(m.D$variable) +
        theme_bw(base_size =16) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- plyr::dlply(m.D, "variable", '%+%', e1 = p1)

setwd(paste(getwd(), "/SDB3", sep = ""))

tit <- at_deg_data$name

x <- length(p2)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""), width = 8, height = 8, units = "in", res = 600)
        print(p2[[i]])
        dev.off()
}
