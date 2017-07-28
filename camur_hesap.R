source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")

d1 <- read.csv("camur_konsantrasyonlar.csv")
d2 <- reshape2::melt(d1, id = c("Tesis", "Mevsim", "Numune"))
d3 <- summarySEwithin(d2, measurevar = "value", 
                      withinvars = c("Tesis", "Mevsim", "Numune", "variable"),
                      idvar = "variable", na.rm = TRUE)
names(d3)[6] <- paste("concentration")
names(d3)[4] <- paste("compound")
d4 <- dplyr::select(d3, Tesis:compound, concentration, sd)
d5 <- dplyr::filter(d4, concentration != 0)
xlsx::write.xlsx(d5, "camur_sonuc.xlsx")
