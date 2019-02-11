library(ggplot2)
library(RColorBrewer)

dt <- read.csv("MTL_consumption_V2.csv")

dt_tidy <- reshape2::melt(dt, id = c("Date", "Flowrate", "P_BOD", "day", "week", "Location"))

p1 <- plyr::dlply(dt_tidy, "variable", function(x){
        ggplot(x, aes(x = variable, y = value)) +
                geom_boxplot(outlier.shape = NA) +
                geom_jitter(aes(color = day, shape = Location), size = 4) +
                scale_color_manual(values = c("#013220", "#E41A1C")) +
                labs(x = "", y = "Consumption (mg/day/1000 inhabitants)") +
                ggtitle(x$variable) +
                theme_bw(base_size = 24)
})

x <- length(p1)
tit <- unique(dt_tidy$variable)

for (i in 1:x){
        png(file = paste(tit[[i]], "_boxplot.png", sep = ""), height = 10, width = 6, res = 600, unit = "in")
        print(p1[[i]])
        dev.off()
}
