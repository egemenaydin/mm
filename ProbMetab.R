library(ProbMetab)
library(dplyr)
Pr_annot <- get.annot(anN, polarity = "negative", minint = 500)
Pr_annot_molion <- Pr_annot$molIon
Pr_annot_base <- filter(Pr_annot_molion, isotope == 0)
masses_N_matrix <- as.data.frame(masses_N)
masses_N_unique <- distinct(select(masses_N_matrix, mass))
write.csv(Pr_annot_base, file = "Pr_annot_base.csv")
write.csv(masses_N, file = "masses_N.csv")

ionAnnotP2plus <- get.annot(anP, allowMiss = TRUE, xset = xset3P) 
ionAnnotN2plus <- get.annot(anN,  polarity="negative", allowMiss=TRUE, xset = xset3N)
combined <- combineMolIon(ionAnnotN2plus, ionAnnotP2plus)
DB <- KEGGcpds
reactionM <- create.reactionM(DB, combined, ppm.tol = 8)
length(unique(reactionM[reactionM[,"id"]!="unknown",1])) 
sum(table(reactionM[reactionM[,"id"]!="unknown",1])>1)
isoPatt <- incorporate.isotopes(combined, reactionM, comb = 1, DB = DB)
w1 <- weightM(isoPatt, intervals = seq(0, 1000, by = 500), offset = c(3.115712, 3.434146, 2.350798))
w <- design.connection(reactionM)

reactionM_dataframe <- as.data.frame(reactionM)
exp_masses <- select(reactionM_dataframe, rt, massObs)
reac_matrix <- matrix(0, ncol = 4)
ppm.tol <- 10
for(i in 1:nrow(exp_masses)) {
        logical <- abs(((exp_masses[i,2] - db.mass)/db.mass)*10^6) < ppm.tol
        if(sum(logical)) {
                reac_matrix0 <- cbind(matrix(exp_masses[i, ], nrow = 1),
                                      as.matrix(valid_formulas[logical, c("mass", "id")]))
        }
        reac_matrix <- rbind(reac_matrix, reac_matrix0)
}
reac_matrix <- reac_matrix[-1, ]