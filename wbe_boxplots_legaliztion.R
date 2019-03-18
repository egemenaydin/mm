library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(ggsci)

dt <- read.csv("MTL_SHB_consumption_V4.csv")

dt_tidy <- reshape2::melt(dt, id = c("Date", "Flowrate", "P_BOD", "day", "week", "Location", "Legal"))

dt_tidy$Legal <- factor(dt_tidy$Legal, levels = rev(levels(dt_tidy$Legal)))

options(scipen = 100000)


ggplot(dt_tidy, aes(x = Location, y = value, color = Legal)) +
        geom_boxplot(outlier.shape = NA, aes(color = Legal)) +
        geom_point(position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.75)) +
        scale_fill_jco() +
        scale_color_jco() +
        labs(x = "", y = "Consumption (mg/day/1000 inhabitants)") +
        facet_wrap(~variable, scales = "free_y")  +
        stat_compare_means(aes(group = Legal), label = "p.signif", size = 8, 
                           color = "red", hide.ns = T) +
        theme_minimal(base_size = 22) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
              legend.title = element_blank(),
              legend.position = "bottom")

ggsave("legalization_comp.png", height = 10, width = 12, dpi = 600, unit = "in")