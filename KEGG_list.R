library(dplyr)
N_all_ionization <- read.csv("negative_all_int.csv")
P_all_ionization <- read.csv("positive_all_int.csv")
all <- rbind(N_all_ionization, P_all_ionization)
all_unique <- distinct(all, mass)
write.csv(all_unique, "all.csv")

data <- read.csv("mz.csv")
ppm.limit <- 10

sup <- as.matrix(data)

DB <- read.csv("KEGG_DB.csv")
DB <- filter(DB, KEGGid != "")
db.mass <- DB$Mass

KEGG_comp <- matrix("", ncol = 5)

for(i in 1:nrow(data)) {
        mass <- sup[i, 1]
        logical <- abs(((mass - db.mass)/db.mass)*10^6) < ppm.limit
        if(sum(logical)) {
                for(j in 1:length(which(logical))) {
                        KEGG_comp1 <- matrix("", nrow = 1, ncol = 5)
                        KEGG_comp1[1, 1] <- sup[i, 1]
                        KEGG_comp1[1, 2] <- db.mass[which(logical)][j]
                        KEGG_comp1[1, 3] <- as.vector(DB[which(logical), "KEGGid"])[j]
                        KEGG_comp1[1, 4] <- as.vector(DB[which(logical), "Formula"])[j]
                        KEGG_comp1[1, 5] <- as.vector(DB[which(logical), "Pathway"])[j]
                        KEGG_comp <- rbind(KEGG_comp, KEGG_comp1)
                }
                
        }
        else {
                KEGG_comp1 <- matrix("", nrow = 1, ncol = 5) 
                KEGG_comp1[1, 1] <- sup[i, 1]
                KEGG_comp1[1, 2] <- "no match"
                KEGG_comp1[1, 3] <- "no match"
                KEGG_comp1[1, 4] <- "no match"
                KEGG_comp1[1, 5] <- "no match"
                KEGG_comp <- rbind(KEGG_comp, KEGG_comp1)
        }
}

header <- c("mass", "massDB", "KEGG ID", "Formula", "Pathway")
colnames(KEGG_comp) <- header
KEGG_comparison <- data.frame(KEGG_comp[-1, ])
KEGG_comparison <- filter(KEGG_comparison, KEGG.ID != "no match")
KEGG_comparison <- distinct(KEGG_comparison, KEGG.ID)
KEGG_int <- merge(KEGG_comparison, data, by = "peaknum")