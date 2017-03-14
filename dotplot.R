library(ggplot2)
#library(plyr)
source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
d1 <- read.csv("conc.csv")
#d2 <- reshape2::melt(d1, id = c("Numune"))
#when there are replicates use this for summary statistics
d3 <- summarySEwithin(d1, measurevar = "Concentration", withinvars = c("Sample", "Compound", "WWTP"), idvar = "Sample")
#xlsx::write.xlsx(d3, "measurement_results.xlsx")
#tc <- dplyr::filter(d3, Numune == "Tesis Cikis")#
#xlsx::write.xlsx(tc, "tesis_cikis.xlsx"#)##

l <- c("Influent", "Grit chamber", "Primary settling", "Bio-P tank", "Anoxic tank", "Aerobic tank", "Effluent")
my.palette <- c("#000000","#831E3F", "#38D750", "#779BFB", "#7E6E0B", "#399EA4", "#FEFF89", "#ED7028", "#E2B5C9", "#3AEBB8", "#443B86", "#F85093", "#C6FC3C", "#2E5847", "#9A0D0F", "#F6535F", "#E0A7F5", "#603B0F", "#206CCE", "#5C6D05")
d1 <- transform(d1, Sample = ordered(Sample, l))

ggplot(d3, aes(x = Sample, y= Concentration, color = Compound, shape = WWTP)) +
        geom_point(size = 5) +
        xlab("")+
        ylab("Concentration (ng/L)") +
        scale_color_manual(values = my.palette) +
        scale_y_log10(breaks = c(1, 10, 100, 1000, 10000)) +
        theme_bw(base_size =24)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.print(png, "concentrations.png", width = 1024, height = 768)
