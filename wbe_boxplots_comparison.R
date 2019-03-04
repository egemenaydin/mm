library(ggplot2)
library(RColorBrewer)
library(ggpubr)

dt <- read.csv("MTL_SHB_consumption_V2.csv")

dt_tidy <- reshape2::melt(dt, id = c("Date", "Flowrate", "P_BOD", "day", "week", "Location", "Legal"))

my_comparisons <- list(c("MTL_North", "MTL_South"), c("MTL_South", "SHB"), c("MTL_North", "SHB"))

p1 <- plyr::dlply(dt_tidy, "variable", function(x){
        ggplot(x, aes(x = Location, y = value)) +
                geom_boxplot(outlier.shape = NA) +
                stat_boxplot(geom = "errorbar", width = 0.2) +
                geom_point(aes(color = day), size = 3, 
                           position = position_jitterdodge(jitter.width = 0.25, 
                                                           dodge.width = 0.75)) +
                scale_color_brewer(palette="Dark2") +
                labs(x = "", y = "Consumption (mg/day/1000 inhabitants)") +
                ggtitle(x$variable) +
                stat_compare_means(color = "red", size = 8, 
                                   comparisons = my_comparisons,
                                   label = "p.signif") +
                theme_minimal(base_size = 26) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                      legend.title = element_blank())
})


x <- length(p1)
tit <- unique(dt_tidy$variable)

for (i in 1:x){
        png(file = paste(tit[[i]], "_boxplot_sign.png", sep = ""), height = 10, width = 10, res = 600, unit = "in")
        print(p1[[i]])
        dev.off()
}
