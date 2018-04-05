library(dplyr)

data <- read.csv("positive_base_in_feature_extract.csv")
ppm.limit <- 2
data$mass <- data$mz - 1.007276


sup <- as.matrix(select(data, mz, mass))

DB <- read.csv("~/mm/MC-OP-db.csv")
db.mass <- DB$mass

MCOP_comp <- matrix("", ncol = 5)

for(i in 1:nrow(data)) {
        mass <- sup[i, 2]
        logical <- abs(((mass - db.mass)/db.mass)*10^6) < ppm.limit
        if(sum(logical)) {
                for(j in 1:length(which(logical))) {
                        MCOP_comp1 <- matrix("", nrow = 1, ncol = 5)
                        MCOP_comp1[1, 1:2] <- sup[i, 1:2]
                        MCOP_comp1[1, 3] <- db.mass[which(logical)][j]
                        MCOP_comp1[1, 4] <- as.vector(DB[which(logical), "Compound"])[j]
                        MCOP_comp1[1, 5] <- as.vector(DB[which(logical), "Formula"])[j]
                        MCOP_comp <- rbind(MCOP_comp, MCOP_comp1)
                }
                
        }
        else {
                MCOP_comp1 <- matrix("", nrow = 1, ncol = 5) 
                MCOP_comp1[1, 1:2] <- sup[i, 1:2]
                MCOP_comp1[1, 3] <- "no match"
                MCOP_comp1[1, 4] <- "no match"
                MCOP_comp1[1, 5] <- "no match"
                MCOP_comp <- rbind(MCOP_comp, MCOP_comp1)
        }
}

header <- c("mz", "mass", "massDB", "Compound","Formula")
colnames(MCOP_comp) <- header
MCOP_comparison <- data.frame(MCOP_comp[-1, ])
MCOP_int <- merge(MCOP_comparison, data, by = "mz")
MCOP_int2 <- filter(MCOP_int, Compound != "no match")

xlsx::write.xlsx(MCOP_int2, "MC_OPdatabasehit.xlsx")

df <- select(MCOP_int2, Compound, blank10:AT0_tp6c)
df2 <- reshape2::melt(df, id = "Compound")
write.csv(df2, "df2.csv")
