library(dplyr)
library(ggplot2)
library(gganimate)
library(magick)

t0 <- read.csv("FTF76_mb_t0.csv")
t2 <- read.csv("FTF76_mb_t2.csv")

t0t2 <- rbind(t0, t2)
rn <- unique(t0t2$X)

baseC <- read.csv("base_in_feature_extract.csv")
baseC <- slice(baseC, rn)
baseC_long <- select(baseC, mz, rt, FE9.tp0.a:FE18.tp4.b)
write.csv(baseC_long, "FTF76_rsw.csv")
baseC <- select(baseC, mz, rt, FE9.tp0.a:FE9.tp4.b)
colnames(baseC) <- c("mz", "rt", "Day0a", "Day0b", "Day20a", "Day20b", "Day40a", "Day40b")

baseC <- mutate(baseC, Day0 = Day0a+Day0b/2)
baseC <- mutate(baseC, Day20 = Day20a+Day20b/2)
baseC <- mutate(baseC, Day40 = Day40a+Day40b/2)

baseC <- mutate(baseC, logDay0 = log2(Day0))
baseC <- mutate(baseC, logDay20 = log2(Day20))
baseC <- mutate(baseC, logDay40 = log2(Day40))

baseC$mass <- baseC$mz - 1.007276

baseC_an <- select(baseC, mass, rt, logDay0:logDay40)
baseC_an2 <- select(baseC, mass, mz, rt, logDay0:logDay40)
colnames(baseC_an) <- c("mass", "rt", "Day 0", "Day 20", "Day 40")
colnames(baseC_an2) <- c("mass", "mz","rt", "Day0", "Day20", "Day40")
baseC_an_long <- reshape2::melt(baseC_an, id = c("mass", "rt"))

p <- ggplot(baseC_an_long, aes(x = rt/60, y = mass, color = value, size = value, frame = variable)) +
        geom_point(position = "jitter", show.legend = FALSE) +
        scale_size("value", guide = FALSE) +
        scale_colour_gradient(low = "green", high ="red", name = "Norm. abundance", limits = c(7,23)) +
        theme_bw(base_size = 16) +
        xlab("Retention time (min)") +
        ylab("Mass (Da)") 

gganimate(p, interval = .7, fps = 30)

gganimate(p, "output_FTF76.gif", interval = .7, fps = 60)

#find consumed and produced compounds

consumed <- filter(baseC_an2, Day20 < Day0 | Day40 < Day0)
consumed_long <- filter(baseC, mz %in% consumed$mz)
consumed_long$D20overD0 <- consumed_long$Day20/consumed_long$Day0
consumed_long$D40overD0 <- consumed_long$Day40/consumed_long$Day0
write.csv(consumed_long, "FTF76_consumed.csv")
produced_long <- subset(baseC, !(mz %in% consumed$mz))
produced_long$D20overD0 <- produced_long$Day20/produced_long$Day0
produced_long$D40overD0 <- produced_long$Day40/produced_long$Day0
write.csv(produced_long, "FTF76_produced.csv")
        