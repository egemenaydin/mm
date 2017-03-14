source("~/mm/summarySEwithin.R")
source("~/mm/normDataWithin.R")
source("~/mm/summarySE.R")
d1 <- read.csv("tum_maddeler_sadece_tesis.csv")
d2 <- reshape2::melt(d1, id = c("Numune"))
#when there are replicates use this for summary statistics
#d3 <- summarySEwithin(d1, measurevar = "concentration", withinvars = c("Numune", "variable", "value", "X"), idvar = "variable")
#xlsx::write.xlsx(d3, "measurement_results.xlsx")
tc <- dplyr::filter(d2, Numune == c("Tesis Cikis", "Tesis Giris"))
tc2 <- summarySEwithin(tc, measurevar = "value", withinvars = c("Numune", "variable"), idvar = "variable")
tc3 <- dplyr::select(tc2, Numune, variable, value)

efficiency <- plyr::ddply(tc, "variable", function (x ,col){
        c(
               efficiency = (x[1,3] - x[2,3])*100/x[1,3] 
        )
        },
        "value"
        )
colnames(efficiency) <- c("Madde", "Giderim verimi(%)")
xlsx::write.xlsx(efficiency, file = "tuzla_giderim.xlsx")
