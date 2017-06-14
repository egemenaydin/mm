library(ggplot2)

d1 <- read.csv("dezenfeksiyon_verim.csv")
d_cl <- dplyr::filter(d1, Dezenfeksiyon == "Klorlama")
d_clnh3 <- dplyr::filter(d1, Dezenfeksiyon == "Kloraminleme")
d_clnh3_2 <- dplyr::filter(d1, Dezenfeksiyon == "Istemsiz Kloraminleme")

ggplot(d_clnh3_2, aes(x = Madde, y = Verim)) +
        geom_boxplot() +
        geom_jitter(aes(color = Mevsim, shape = Tesis), size = 3) +
        scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
        labs(x = "", y = "Giderim verimi (%)") +
        theme_bw(base_size = 26) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
        

dev.print(png, "verim-boxplot-istemsiz-koloraminleme.png", height = 9, width = 16, res = 600, unit = "in")

"#984EA3", "#E41A1C", "#377EB8", "#4DAF4A"