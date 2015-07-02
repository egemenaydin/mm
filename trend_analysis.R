library(dplyr)
library(ggplot2)
library(RColorBrewer)

#RSW Camelina

df <- read.csv("positive_base_int.csv")
df <- select(df, mz, rt, FE4.tp0.a:FE20.tp4.b)

df <- baseC_long

data.frame(df, df$Camelina.tp0 <- apply(select(df, FE5.tp0.a, FE5.tp0.b), 1, mean))
data.frame(df, df$Camelina.tp1 <- apply(select(df, FE5.tp2.a, FE5.tp2.b), 1, mean))
data.frame(df, df$Camelina.tp2 <- apply(select(df, FE5.tp4.a, FE5.tp4.b), 1, mean))
data.frame(df, df$No.fuel.tp0 <- apply(select(df, FE20.tp0.a, FE20.tp0.b), 1, mean))
data.frame(df, df$No.fuel.tp1 <- apply(select(df, FE20.tp2.a, FE20.tp2.b), 1, mean))
data.frame(df, df$No.fuel.tp2 <- apply(select(df, FE20.tp4.a, FE20.tp4.b), 1, mean))

data.frame(df, df$Camelina.abiotic.tp0 <- apply(select(df, FE4.tp0.a, FE4.tp0.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp1 <- apply(select(df, FE4.tp2.a, FE4.tp2.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp2 <- apply(select(df, FE4.tp4.a, FE4.tp4.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp0 <- apply(select(df, FE19.tp0.a, FE19.tp0.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp1 <- apply(select(df, FE19.tp2.a, FE19.tp2.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp2 <- apply(select(df, FE19.tp4.a, FE19.tp4.b), 1, mean))

data.frame(df, df$Camelina.tp0.sd <- apply(select(df, FE5.tp0.a, FE5.tp0.b), 1, sd))
data.frame(df, df$Camelina.tp1.sd <- apply(select(df, FE5.tp2.a, FE5.tp2.b), 1, sd))
data.frame(df, df$Camelina.tp2.sd <- apply(select(df, FE5.tp4.a, FE5.tp4.b), 1, sd))
data.frame(df, df$No.fuel.tp0.sd <- apply(select(df, FE20.tp0.a, FE20.tp0.b), 1, sd))
data.frame(df, df$No.fuel.tp1.sd <- apply(select(df, FE20.tp2.a, FE20.tp2.b), 1, sd))
data.frame(df, df$No.fuel.tp2.sd <- apply(select(df, FE20.tp4.a, FE20.tp4.b), 1, sd))

data.frame(df, df$Camelina.abiotic.tp0.sd <- apply(select(df, FE4.tp0.a, FE4.tp0.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp1.sd <- apply(select(df, FE4.tp2.a, FE4.tp2.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp2.sd <- apply(select(df, FE4.tp4.a, FE4.tp4.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp0.sd <- apply(select(df, FE19.tp0.a, FE19.tp0.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp1.sd <- apply(select(df, FE19.tp2.a, FE19.tp2.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp2.sd <- apply(select(df, FE19.tp4.a, FE19.tp4.b), 1, sd))

df_decrease <- filter(df, Camelina.tp0 / Camelina.tp1 > 3 | Camelina.tp0 / Camelina.tp2 > 3 & Camelina.tp0 / No.fuel.tp2 >3)
df_increase <- filter(df, Camelina.tp1 / Camelina.tp0 > 3 | Camelina.tp2 / Camelina.tp0 > 3 & Camelina.tp2 / No.fuel.tp2 >3)

write.csv(df_decrease, "decrease.csv")
write.csv(df_increase, "increase.csv")

df_decrease$trend <- "decrease"
df_increase$trend <- "increase"
df2 <- rbind(df_decrease, df_increase)
write.csv(df2, "increase_decrease.csv")

p <- ggplot(df2, aes(x = mz))
p +  geom_histogram(binwidth = 10)+ facet_grid(trend~.) + theme_bw(base_size = 18)


dev.print(pdf, "camelina_increase_decrease_hist.pdf", height = 5, width = 10)

inc <- read.csv("increase2.csv", check.names = FALSE)
dec <- read.csv("decrease2.csv", check.names = FALSE)

setwd("~/mm/decrease/")

tit <- names(dec)
tit <- tit[(-c(1, 2))]

m <- reshape2::melt(dec, id = c("Fuel", "time"))
p <- ggplot(m, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1 <- plyr::dlply(m, "variable", '%+%', e1 = p)

x <- length(p1)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

setwd("~/mm/increase/")

tit_ic <- names(inc)
tit_ic <- tit_ic[(-c(1, 2))]

ic <- reshape2::melt(inc, id = c("Fuel", "time"))
p_ic <- ggplot(ic, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1_ic <- plyr::dlply(ic, "variable", '%+%', e1 = p_ic)

x_ic <- length(p1_ic)

for (i in 1:x_ic){
        png(file = paste(tit_ic[[i]], ".png", sep = ""))
        print(p1_ic[[i]])
        dev.off()
}


#raw F76

library(dplyr)
library(ggplot2)
library(RColorBrewer)
df <- read.csv("positive_base_int.csv")
df <- select(df, mz, rt, FE4.tp0.a:FE20.tp4.b)
df <- baseC_long

data.frame(df, df$Camelina.tp0 <- apply(select(df, FE15.tp0.a, FE15.tp0.b), 1, mean))
data.frame(df, df$Camelina.tp1 <- apply(select(df, FE15.tp2.a, FE15.tp2.b), 1, mean))
data.frame(df, df$Camelina.tp2 <- apply(select(df, FE15.tp4.a, FE15.tp4.b), 1, mean))
data.frame(df, df$No.fuel.tp0 <- apply(select(df, FE20.tp0.a, FE20.tp0.b), 1, mean))
data.frame(df, df$No.fuel.tp1 <- apply(select(df, FE20.tp2.a, FE20.tp2.b), 1, mean))
data.frame(df, df$No.fuel.tp2 <- apply(select(df, FE20.tp4.a, FE20.tp4.b), 1, mean))

data.frame(df, df$Camelina.abiotic.tp0 <- apply(select(df, FE14.tp0.a, FE14.tp0.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp1 <- apply(select(df, FE14.tp2.a, FE14.tp2.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp2 <- apply(select(df, FE14.tp4.a, FE14.tp4.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp0 <- apply(select(df, FE19.tp0.a, FE19.tp0.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp1 <- apply(select(df, FE19.tp2.a, FE19.tp2.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp2 <- apply(select(df, FE19.tp4.a, FE19.tp4.b), 1, mean))

data.frame(df, df$Camelina.tp0.sd <- apply(select(df, FE15.tp0.a, FE15.tp0.b), 1, sd))
data.frame(df, df$Camelina.tp1.sd <- apply(select(df, FE15.tp2.a, FE15.tp2.b), 1, sd))
data.frame(df, df$Camelina.tp2.sd <- apply(select(df, FE15.tp4.a, FE15.tp4.b), 1, sd))
data.frame(df, df$No.fuel.tp0.sd <- apply(select(df, FE20.tp0.a, FE20.tp0.b), 1, sd))
data.frame(df, df$No.fuel.tp1.sd <- apply(select(df, FE20.tp2.a, FE20.tp2.b), 1, sd))
data.frame(df, df$No.fuel.tp2.sd <- apply(select(df, FE20.tp4.a, FE20.tp4.b), 1, sd))

data.frame(df, df$Camelina.abiotic.tp0.sd <- apply(select(df, FE14.tp0.a, FE14.tp0.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp1.sd <- apply(select(df, FE14.tp2.a, FE14.tp2.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp2.sd <- apply(select(df, FE14.tp4.a, FE14.tp4.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp0.sd <- apply(select(df, FE19.tp0.a, FE19.tp0.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp1.sd <- apply(select(df, FE19.tp2.a, FE19.tp2.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp2.sd <- apply(select(df, FE19.tp4.a, FE19.tp4.b), 1, sd))

df_decrease <- filter(df, Camelina.tp0 / Camelina.tp1 > 3 | Camel/home/egemen/Desktop/Oklahoma/fuel-extract/degradation/RSW-camelina/volcano/base_in_feature_extract.csvina.tp0 / Camelina.tp2 > 3 & Camelina.tp0 / No.fuel.tp2 >3)
df_increase <- filter(df, Camelina.tp1 / Camelina.tp0 > 3 | Camelina.tp2 / Camelina.tp0 > 3 & Camelina.tp2 / No.fuel.tp2 >3)

write.csv(df_decrease, "decrease.csv")
write.csv(df_increase, "increase.csv")

df_decrease$trend <- "decrease"
df_increase$trend <- "increase"
df2 <- rbind(df_decrease, df_increase)
write.csv(df2, "increase_decrease.csv")

p <- ggplot(df2, aes(x = mz))
p +  geom_histogram(binwidth = 10)+ facet_grid(trend~.) + theme_bw(base_size = 18)


dev.print(pdf, "camelina_increase_decrease_hist.pdf", height = 5, width = 10)

inc <- read.csv("increase2.csv", check.names = FALSE)
dec <- read.csv("decrease2.csv", check.names = FALSE)

setwd("~/mm/decrease/")

tit <- names(dec)
tit <- tit[(-c(1, 2))]

m <- reshape2::melt(dec, id = c("Fuel", "time"))
p <- ggplot(m, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1 <- plyr::dlply(m, "variable", '%+%', e1 = p)

x <- length(p1)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

setwd("~/mm/increase/")

tit_ic <- names(inc)
tit_ic <- tit_ic[(-c(1, 2))]

ic <- reshape2::melt(inc, id = c("Fuel", "time"))
p_ic <- ggplot(ic, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1_ic <- plyr::dlply(ic, "variable", '%+%', e1 = p_ic)

x_ic <- length(p1_ic)

for (i in 1:x_ic){
        png(file = paste(tit_ic[[i]], ".png", sep = ""))
        print(p1_ic[[i]])
        dev.off()
}


#artificial seawater FTF76
df <- read.csv("positive_base_int.csv")
df <- select(df, mz, rt, FE12.tp0.a:FE16.tp4.b)
df <- baseC_long

data.frame(df, df$Camelina.tp0 <- apply(select(df, FE11.tp0.a, FE11.tp0.b), 1, mean))
data.frame(df, df$Camelina.tp1 <- apply(select(df, FE11.tp2.a, FE11.tp2.b), 1, mean))
data.frame(df, df$Camelina.tp2 <- apply(select(df, FE11.tp4.a, FE11.tp4.b), 1, mean))
data.frame(df, df$No.fuel.tp0 <- apply(select(df, FE16.tp0.a, FE16.tp0.b), 1, mean))
data.frame(df, df$No.fuel.tp1 <- apply(select(df, FE16.tp2.a, FE16.tp2.b), 1, mean))
data.frame(df, df$No.fuel.tp2 <- apply(select(df, FE16.tp4.a, FE16.tp4.b), 1, mean))

data.frame(df, df$Camelina.abiotic.tp0 <- apply(select(df, FE12.tp0.a, FE12.tp0.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp1 <- apply(select(df, FE12.tp2.a, FE12.tp2.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp2 <- apply(select(df, FE12.tp4.a, FE12.tp4.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp0 <- apply(select(df, FE17.tp0.a, FE17.tp0.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp1 <- apply(select(df, FE17.tp2.a, FE17.tp2.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp2 <- apply(select(df, FE17.tp4.a, FE17.tp4.b), 1, mean))

data.frame(df, df$Camelina.tp0.sd <- apply(select(df, FE11.tp0.a, FE11.tp0.b), 1, sd))
data.frame(df, df$Camelina.tp1.sd <- apply(select(df, FE11.tp2.a, FE11.tp2.b), 1, sd))
data.frame(df, df$Camelina.tp2.sd <- apply(select(df, FE11.tp4.a, FE11.tp4.b), 1, sd))
data.frame(df, df$No.fuel.tp0.sd <- apply(select(df, FE16.tp0.a, FE16.tp0.b), 1, sd))
data.frame(df, df$No.fuel.tp1.sd <- apply(select(df, FE16.tp2.a, FE16.tp2.b), 1, sd))
data.frame(df, df$No.fuel.tp2.sd <- apply(select(df, FE16.tp4.a, FE16.tp4.b), 1, sd))

data.frame(df, df$Camelina.abiotic.tp0.sd <- apply(select(df, FE12.tp0.a, FE12.tp0.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp1.sd <- apply(select(df, FE12.tp2.a, FE12.tp2.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp2.sd <- apply(select(df, FE12.tp4.a, FE12.tp4.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp0.sd <- apply(select(df, FE17.tp0.a, FE17.tp0.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp1.sd <- apply(select(df, FE17.tp2.a, FE17.tp2.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp2.sd <- apply(select(df, FE17.tp4.a, FE17.tp4.b), 1, sd))

df_decrease <- filter(df, Camelina.tp0 / Camelina.tp1 > 3 | Camelina.tp0 / Camelina.tp2 > 3 & Camelina.tp0 / No.fuel.tp2 >3)
df_increase <- filter(df, Camelina.tp1 / Camelina.tp0 > 3 | Camelina.tp2 / Camelina.tp0 > 3 & Camelina.tp2 / No.fuel.tp2 >3)

write.csv(df_decrease, "decrease.csv")
write.csv(df_increase, "increase.csv")

df_decrease$trend <- "decrease"
df_increase$trend <- "increase"
df2 <- rbind(df_decrease, df_increase)
write.csv(df2, "increase_decrease.csv")

p <- ggplot(df2, aes(x = mz))
p +  geom_histogram(binwidth = 10)+ facet_grid(trend~.) + theme_bw(base_size = 18)


dev.print(pdf, "camelina_increase_decrease_hist.pdf", height = 5, width = 10)

inc <- read.csv("increase2.csv", check.names = FALSE)
dec <- read.csv("decrease2.csv", check.names = FALSE)

setwd("~/GitHub/mm/decrease/")

tit <- names(dec)
tit <- tit[(-c(1, 2))]

m <- reshape2::melt(dec, id = c("Fuel", "time"))
p <- ggplot(m, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1 <- plyr::dlply(m, "variable", '%+%', e1 = p)

x <- length(p1)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

setwd("~/GitHub/mm/increase/")

tit_ic <- names(inc)
tit_ic <- tit_ic[(-c(1, 2))]

ic <- reshape2::melt(inc, id = c("Fuel", "time"))
p_ic <- ggplot(ic, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1_ic <- plyr::dlply(ic, "variable", '%+%', e1 = p_ic)

x_ic <- length(p1_ic)

for (i in 1:x_ic){
        png(file = paste(tit_ic[[i]], ".png", sep = ""))
        print(p1_ic[[i]])
        dev.off()
}



#artificial seawater Camelina
df <- read.csv("positive_base_int.csv")
df <- select(df, mz, rt, FE12.tp0.a:FE16.tp4.b)
df <- baseC_long

data.frame(df, df$Camelina.tp0 <- apply(select(df, FE1.tp0.a, FE1.tp0.b), 1, mean))
data.frame(df, df$Camelina.tp1 <- apply(select(df, FE1.tp2.a, FE1.tp2.b), 1, mean))
data.frame(df, df$Camelina.tp2 <- apply(select(df, FE1.tp4.a, FE1.tp4.b), 1, mean))
data.frame(df, df$No.fuel.tp0 <- apply(select(df, FE16.tp0.a, FE16.tp0.b), 1, mean))
data.frame(df, df$No.fuel.tp1 <- apply(select(df, FE16.tp2.a, FE16.tp2.b), 1, mean))
data.frame(df, df$No.fuel.tp2 <- apply(select(df, FE16.tp4.a, FE16.tp4.b), 1, mean))

data.frame(df, df$Camelina.abiotic.tp0 <- apply(select(df, FE2.tp0.a, FE2.tp0.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp1 <- apply(select(df, FE2.tp2.a, FE2.tp2.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp2 <- apply(select(df, FE2.tp4.a, FE2.tp4.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp0 <- apply(select(df, FE17.tp0.a, FE17.tp0.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp1 <- apply(select(df, FE17.tp2.a, FE17.tp2.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp2 <- apply(select(df, FE17.tp4.a, FE17.tp4.b), 1, mean))

data.frame(df, df$Camelina.tp0.sd <- apply(select(df, FE1.tp0.a, FE1.tp0.b), 1, sd))
data.frame(df, df$Camelina.tp1.sd <- apply(select(df, FE1.tp2.a, FE1.tp2.b), 1, sd))
data.frame(df, df$Camelina.tp2.sd <- apply(select(df, FE1.tp4.a, FE1.tp4.b), 1, sd))
data.frame(df, df$No.fuel.tp0.sd <- apply(select(df, FE16.tp0.a, FE16.tp0.b), 1, sd))
data.frame(df, df$No.fuel.tp1.sd <- apply(select(df, FE16.tp2.a, FE16.tp2.b), 1, sd))
data.frame(df, df$No.fuel.tp2.sd <- apply(select(df, FE16.tp4.a, FE16.tp4.b), 1, sd))

data.frame(df, df$Camelina.abiotic.tp0.sd <- apply(select(df, FE2.tp0.a, FE2.tp0.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp1.sd <- apply(select(df, FE2.tp2.a, FE2.tp2.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp2.sd <- apply(select(df, FE2.tp4.a, FE2.tp4.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp0.sd <- apply(select(df, FE17.tp0.a, FE17.tp0.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp1.sd <- apply(select(df, FE17.tp2.a, FE17.tp2.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp2.sd <- apply(select(df, FE17.tp4.a, FE17.tp4.b), 1, sd))

df_decrease <- filter(df, Camelina.tp0 / Camelina.tp1 > 3 | Camelina.tp0 / Camelina.tp2 > 3 & Camelina.tp0 / No.fuel.tp2 >3)
df_increase <- filter(df, Camelina.tp1 / Camelina.tp0 > 3 | Camelina.tp2 / Camelina.tp0 > 3 & Camelina.tp2 / No.fuel.tp2 >3)

write.csv(df_decrease, "decrease.csv")
write.csv(df_increase, "increase.csv")

df_decrease$trend <- "decrease"
df_increase$trend <- "increase"
df2 <- rbind(df_decrease, df_increase)
write.csv(df2, "increase_decrease.csv")

p <- ggplot(df2, aes(x = mz))
p +  geom_histogram(binwidth = 10)+ facet_grid(trend~.) + theme_bw(base_size = 18)


dev.print(pdf, "camelina_increase_decrease_hist.pdf", height = 5, width = 10)

inc <- read.csv("increase2.csv", check.names = FALSE)
dec <- read.csv("decrease2.csv", check.names = FALSE)

setwd("~/GitHub/mm/decrease/")

tit <- names(dec)
tit <- tit[(-c(1, 2))]

m <- reshape2::melt(dec, id = c("Fuel", "time"))
p <- ggplot(m, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1 <- plyr::dlply(m, "variable", '%+%', e1 = p)

x <- length(p1)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

setwd("~/GitHub/mm/increase/")

tit_ic <- names(inc)
tit_ic <- tit_ic[(-c(1, 2))]

ic <- reshape2::melt(inc, id = c("Fuel", "time"))
p_ic <- ggplot(ic, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1_ic <- plyr::dlply(ic, "variable", '%+%', e1 = p_ic)

x_ic <- length(p1_ic)

for (i in 1:x_ic){
        png(file = paste(tit_ic[[i]], ".png", sep = ""))
        print(p1_ic[[i]])
        dev.off()
}

#artificial seawater FTF76
df <- read.csv("positive_base_int.csv")
df <- select(df, mz, rt, FE12.tp0.a:FE16.tp4.b)
df <- baseC_long

data.frame(df, df$Camelina.tp0 <- apply(select(df, FE6.tp0.a, FE6.tp0.b), 1, mean))
data.frame(df, df$Camelina.tp1 <- apply(select(df, FE6.tp2.a, FE6.tp2.b), 1, mean))
data.frame(df, df$Camelina.tp2 <- apply(select(df, FE6.tp4.a, FE6.tp4.b), 1, mean))
data.frame(df, df$No.fuel.tp0 <- apply(select(df, FE16.tp0.a, FE16.tp0.b), 1, mean))
data.frame(df, df$No.fuel.tp1 <- apply(select(df, FE16.tp2.a, FE16.tp2.b), 1, mean))
data.frame(df, df$No.fuel.tp2 <- apply(select(df, FE16.tp4.a, FE16.tp4.b), 1, mean))

data.frame(df, df$Camelina.abiotic.tp0 <- apply(select(df, FE7.tp0.a, FE7.tp0.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp1 <- apply(select(df, FE7.tp2.a, FE7.tp2.b), 1, mean))
data.frame(df, df$Camelina.abiotic.tp2 <- apply(select(df, FE7.tp4.a, FE7.tp4.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp0 <- apply(select(df, FE17.tp0.a, FE17.tp0.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp1 <- apply(select(df, FE17.tp2.a, FE17.tp2.b), 1, mean))
data.frame(df, df$No.fuel.abiotic.tp2 <- apply(select(df, FE17.tp4.a, FE17.tp4.b), 1, mean))

data.frame(df, df$Camelina.tp0.sd <- apply(select(df, FE6.tp0.a, FE6.tp0.b), 1, sd))
data.frame(df, df$Camelina.tp1.sd <- apply(select(df, FE6.tp2.a, FE6.tp2.b), 1, sd))
data.frame(df, df$Camelina.tp2.sd <- apply(select(df, FE6.tp4.a, FE6.tp4.b), 1, sd))
data.frame(df, df$No.fuel.tp0.sd <- apply(select(df, FE16.tp0.a, FE16.tp0.b), 1, sd))
data.frame(df, df$No.fuel.tp1.sd <- apply(select(df, FE16.tp2.a, FE16.tp2.b), 1, sd))
data.frame(df, df$No.fuel.tp2.sd <- apply(select(df, FE16.tp4.a, FE16.tp4.b), 1, sd))

data.frame(df, df$Camelina.abiotic.tp0.sd <- apply(select(df, FE7.tp0.a, FE7.tp0.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp1.sd <- apply(select(df, FE7.tp2.a, FE7.tp2.b), 1, sd))
data.frame(df, df$Camelina.abiotic.tp2.sd <- apply(select(df, FE7.tp4.a, FE7.tp4.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp0.sd <- apply(select(df, FE17.tp0.a, FE17.tp0.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp1.sd <- apply(select(df, FE17.tp2.a, FE17.tp2.b), 1, sd))
data.frame(df, df$No.fuel.abiotic.tp2.sd <- apply(select(df, FE17.tp4.a, FE17.tp4.b), 1, sd))

df_decrease <- filter(df, Camelina.tp0 / Camelina.tp1 > 3 | Camelina.tp0 / Camelina.tp2 > 3 & Camelina.tp0 / No.fuel.tp2 >3)
df_increase <- filter(df, Camelina.tp1 / Camelina.tp0 > 3 | Camelina.tp2 / Camelina.tp0 > 3 & Camelina.tp2 / No.fuel.tp2 >3)

write.csv(df_decrease, "decrease.csv")
write.csv(df_increase, "increase.csv")

df_decrease$trend <- "decrease"
df_increase$trend <- "increase"
df2 <- rbind(df_decrease, df_increase)
write.csv(df2, "increase_decrease.csv")

p <- ggplot(df2, aes(x = mz))
p +  geom_histogram(binwidth = 10)+ facet_grid(trend~.) + theme_bw(base_size = 18)


dev.print(pdf, "camelina_increase_decrease_hist.pdf", height = 5, width = 10)

inc <- read.csv("increase2.csv", check.names = FALSE)
dec <- read.csv("decrease2.csv", check.names = FALSE)

setwd("~/GitHub/mm/decrease/")

tit <- names(dec)
tit <- tit[(-c(1, 2))]

m <- reshape2::melt(dec, id = c("Fuel", "time"))
p <- ggplot(m, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1 <- plyr::dlply(m, "variable", '%+%', e1 = p)

x <- length(p1)

for (i in 1:x){
        png(file = paste(tit[[i]], ".png", sep = ""))
        print(p1[[i]])
        dev.off()
}

setwd("~/GitHub/mm/increase/")

tit_ic <- names(inc)
tit_ic <- tit_ic[(-c(1, 2))]

ic <- reshape2::melt(inc, id = c("Fuel", "time"))
p_ic <- ggplot(ic, aes(x = time, y = value, color = Fuel, group = Fuel)) +
        geom_point(size = 4) + 
        scale_color_brewer(type = "qual", palette = "Set1") +
        xlab("Time (days)") +
        ylab("Intensity (counts)") +
        theme_bw(base_size =16)
p1_ic <- plyr::dlply(ic, "variable", '%+%', e1 = p_ic)

x_ic <- length(p1_ic)

for (i in 1:x_ic){
        png(file = paste(tit_ic[[i]], ".png", sep = ""))
        print(p1_ic[[i]])
        dev.off()
}









