library(ggplot2)
data <- read.csv("sample.csv")

data[data == 0] <- NA

windowsFonts(Times=windowsFont("TT Times New Roman"))

my.palette <- c("#000000", "#F0E442", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7")

ggplot(data, aes(x = Sample, y = Abundance)) + 
        geom_rect(data=NULL,aes(xmin=0.25,xmax=1.5,ymin=-Inf,ymax=Inf), fill="#000000", alpha = 0.05) +
        geom_rect(data=NULL,aes(xmin=1.5,xmax=2.5,ymin=-Inf,ymax=Inf), fill="#F0E442", alpha = 0.05) +
        geom_rect(data=NULL,aes(xmin=2.5,xmax=6.5,ymin=-Inf,ymax=Inf), fill="#56B4E9", alpha = 0.05) +
        geom_rect(data=NULL,aes(xmin=6.5,xmax=10.5,ymin=-Inf,ymax=Inf), fill="#009E73", alpha = 0.05) +
        geom_rect(data=NULL,aes(xmin=10.5,xmax=14.75,ymin=-Inf,ymax=Inf), fill="#D55E00", alpha = 0.05) +
        scale_color_manual(values = my.palette) +
        geom_point(size = 3, color = "black", shape = 15) + 
        geom_point(size = 0.01, aes(color = Category), shape = 1, cex = 0.0001) +
        guides(colour = guide_legend(override.aes = list(shape = 15, size = 5))) +
        theme_bw(base_size =16, base_family = "Times") + 
        geom_errorbar(aes(ymax = Abundance + Sdev, ymin = Abundance - Sdev), width = 0.30) + 
        theme(axis.text.x = element_text(angle = 90, hjust = -0.25, vjust = 0.5)) + 
        scale_x_discrete(limits = (data$Sample)[order(data$Category)]) + 
        annotate("text", x = 1, y = 40000, label = "Not detected", angle = 90, size = 7, family = "Times") +
        annotate("text", x = 4:6, y = 40000, label = "Not detected", angle = 90, size = 7, family = "Times") + 
        annotate( "text", x = 8, y = 40000, label = "Not detected", angle = 90, size = 7, family = "Times") + 
        annotate( "text", x = 11:14, y = 40000, label = "Not detected", angle = 90, size = 7, family = "Times") 
        
dev.print(tiff, file = "sulfolane-abundance-change.tif", res = 600, height = 8, width = 10, units = "in")
