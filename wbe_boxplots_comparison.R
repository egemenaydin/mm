library(ggplot2)
library(RColorBrewer)

dt <- read.csv("MTL_SHB_consumption_V1.csv")

dt_tidy <- reshape2::melt(dt, id = c("Date", "Flowrate", "P_BOD", "day", "week", "Location"))

p1 <- plyr::dlply(dt_tidy, "variable", function(x){
        ggplot(x, aes(x = Location, y = value)) +
                geom_boxplot(outlier.shape = NA) +
                geom_jitter(aes(color = day), size = 3) +
                scale_color_brewer(palette="Dark2") +
                labs(x = "", y = "Consumption (mg/day/1000 inhabitants)") +
                ggtitle(x$variable)  +
                theme_bw(base_size = 24) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
})

x <- length(p1)
tit <- unique(dt_tidy$variable)

for (i in 1:x){
        png(file = paste(tit[[i]], "_boxplot.png", sep = ""), height = 10, width = 6, res = 600, unit = "in")
        print(p1[[i]])
        dev.off()
}
