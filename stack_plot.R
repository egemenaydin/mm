library(ggplot2)
library(RColorBrewer)

df <- read.csv("chloramine_species_long.csv")
df$Species <- factor(df$Species, levels = c("Trichloramine", "Dichloramine", "Monochloramine", "Free chlorine"))

ggplot(df, aes(Time, Concentration, fill = Species))+
        geom_bar(stat = "identity", position = "stack")+
        facet_grid( ~ Sample)+
        scale_x_continuous(breaks = c(0, 72))+
        ylab(expression(paste("Concentration (mg ", Cl[2], "/L)", sep = "")))+
        xlab("Time (h)")+
        theme_bw(base_size =16)+
        scale_fill_brewer(palette = "Set1")

dev.print(png, file = "chloramine_species3.png", width = 8, height = 4.5, units = "in", res = 600)

df2 <- read.csv("NDMA_pH.csv")

ggplot(df2, aes(Sample, Concentration))+
        geom_bar(stat = "identity")+
        geom_errorbar(position = position_dodge(), 
                      aes(ymin = Concentration - STDEV, ymax = Concentration + STDEV), width = 0.3)+
        theme_bw(base_size =16)+
        ylab("Concentration (ng/L)")+
        scale_x_discrete(limits = c(1:8))

dev.print(png, file = "NDMA_pH.png", width = 8, height = 4.5, units = "in", res = 600)

df3 <- read.csv("NDMA_pH_comp.csv")
df3$pH <- factor(df3$pH, levels = c("5", "8"))
df3$Time <- factor(df3$Time, levels = c("0", "2", "24", "48", "72"))

ggplot(df3, aes(Time, Concentration, fill = pH)) +
        geom_bar(stat = "identity", position = "dodge")+
        geom_errorbar(position = position_dodge(0.9), 
                      aes(ymin = Concentration - STDEV, ymax = Concentration + STDEV), width = 0.3)+
        theme_bw(base_size =16)+
        xlab("Time (h)")+
        ylab("Concentration (ng/L)")+
        scale_fill_brewer(palette = "Set1")+
        theme_bw(base_size =16)

dev.print(png, file = "NDMA_pH_comp.png", width = 8, height = 4.5, units = "in", res = 600)
