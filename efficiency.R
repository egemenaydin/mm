source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
d1 <- read.csv("tesis_tuzla_sb.csv")
d2 <- reshape2::melt(d1, id = c("Numune"))
#when there are replicates use this for summary statistics
#d3 <- summarySEwithin(d1, measurevar = "concentration", withinvars = c("Numune", "variable", "value", "X"), idvar = "variable")
#xlsx::write.xlsx(d3, "measurement_results.xlsx")

tc <- summarySEwithin(dplyr::filter(d2, !is.na(value)), measurevar = "value", withinvars = c("Numune", "variable"), idvar = "Numune")
tc2 <- dplyr::filter(tc, Numune == "Tesis Cikis" | Numune == "Tesis Giris")
tc3 <- dplyr::select(tc2, Numune, variable, value, sd)
colnames(tc3)[2] <- "Madde"

efficiency <- plyr::ddply(tc3, "Madde", function (x ,col){
        c(
               efficiency = (x[2,3] - x[1,3])*100/x[2,3] 
        )
        },
        "value"
        )
colnames(efficiency) <- c("Madde", "Giderim verimi(%)")
xlsx::write.xlsx(efficiency, file = "tuzla_kis_giderim.xlsx")
xlsx::write.xlsx(tc3, file = "tuzla_kis_konsantrasyonlar.xlsx")
