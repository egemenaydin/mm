library(ggplot2)

d1 <- read.csv("verim_karsilastirma.csv")

p1 <- plyr::dlply(d1, "Madde", function(x){
        ggplot(x, aes(x = Aritim, y = Verim)) +
                geom_boxplot() +
                geom_jitter(aes(color = Mevsim, shape = Tesis), size = 3) +
                scale_color_manual(values = c("#984EA3", "#E41A1C", "#377EB8", "#4DAF4A")) +
                labs(x = "", y = "Giderim verimi (%)") +
                ggtitle(x$Madde) +
                theme_bw(base_size = 16) +
                scale_y_continuous(limits = c(-5,105)) +
                theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
})

x <- length(p1)
tit <- unique(d1$Madde)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}
