library(dplyr)
N_all_ionization <- read.csv("negative_all_int.csv")
P_all_ionization <- read.csv("positive_all_int.csv")
all <- rbind(N_all_ionization, P_all_ionization)
all_unique <- distinct(all, mass)
write.csv(all_unique, "all.csv")

data <- read.csv("all_ions.csv")
ppm.limit <- 10

sup <- as.matrix(select(data, mz, mass))

DB <- read.csv("~/mm/KEGG_DB.csv")
DB <- filter(DB, KEGGid != "")
db.mass <- DB$Mass

KEGG_comp <- matrix("", ncol = 6)

for(i in 1:nrow(data)) {
        mass <- sup[i, 2]
        logical <- abs(((mass - db.mass)/db.mass)*10^6) < ppm.limit
        if(sum(logical)) {
                for(j in 1:length(which(logical))) {
                        KEGG_comp1 <- matrix("", nrow = 1, ncol = 6)
                        KEGG_comp1[1, 1:2] <- sup[i, 1:2]
                        KEGG_comp1[1, 3] <- db.mass[which(logical)][j]
                        KEGG_comp1[1, 4] <- as.vector(DB[which(logical), "KEGGid"])[j]
                        KEGG_comp1[1, 5] <- as.vector(DB[which(logical), "Formula"])[j]
                        KEGG_comp1[1, 6] <- as.vector(DB[which(logical), "Pathway"])[j]
                        KEGG_comp <- rbind(KEGG_comp, KEGG_comp1)
                }
                
        }
        else {
                KEGG_comp1 <- matrix("", nrow = 1, ncol = 6) 
                KEGG_comp1[1, 1:2] <- sup[i, 1:2]
                KEGG_comp1[1, 3] <- "no match"
                KEGG_comp1[1, 4] <- "no match"
                KEGG_comp1[1, 5] <- "no match"
                KEGG_comp1[1, 6] <- "no match"
                KEGG_comp <- rbind(KEGG_comp, KEGG_comp1)
        }
}

header <- c("mz", "mass", "massDB", "KEGG ID", "Formula", "Pathway")
colnames(KEGG_comp) <- header
KEGG_comparison <- data.frame(KEGG_comp[-1, ])
KEGG_int <- merge(KEGG_comparison, data, by = "mz")

KEGG_int_for_pathview <- filter(KEGG_int, KEGG.ID != "no match")
KEGG_int_for_pathview_2 <- select(KEGG_int_for_pathview, KEGG.ID, MW1a:MW8c)
KEGG_int_for_pathview_2 <- distinct(KEGG_int_for_pathview_2, KEGG.ID)
rownames(KEGG_int_for_pathview_2) <- KEGG_int_for_pathview_2$KEGG.ID
KEGG_int_for_pathview_2$KEGG.ID <- NULL
# KEGG_int_for_pathview_2$D1 <- apply(select(KEGG_int_for_pathview_2, D1.r001:D1.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$D2 <- apply(select(KEGG_int_for_pathview_2, D2.r001:D2.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$D3 <- apply(select(KEGG_int_for_pathview_2, D3.r001:D3.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$D4 <- apply(select(KEGG_int_for_pathview_2, D4.r001:D4.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$D5 <- apply(select(KEGG_int_for_pathview_2, D5.r001:D5.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$D6 <- apply(select(KEGG_int_for_pathview_2, D6.r001:D6.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$DS1 <- apply(select(KEGG_int_for_pathview_2, DS1.r001:DS1.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$ID1 <- apply(select(KEGG_int_for_pathview_2, ID1.r001:ID1.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$N1 <- apply(select(KEGG_int_for_pathview_2, N1.r001:N1.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$N2 <- apply(select(KEGG_int_for_pathview_2, N2.r001:N2.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$N3 <- apply(select(KEGG_int_for_pathview_2, N3.r001:N3.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$N4 <- apply(select(KEGG_int_for_pathview_2, N4.r001:N4.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$N5 <- apply(select(KEGG_int_for_pathview_2, N5.r001:N5.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$N6 <- apply(select(KEGG_int_for_pathview_2, N6.r001:N6.r003), 1, function(x) mean(x))
# KEGG_int_for_pathview_2$NS1 <- apply(select(KEGG_int_for_pathview_2, NS1.r001:NS1.r003), 1, function(x) mean(x))

KEGG_int_for_pathview_2$MW1 <- apply(select(KEGG_int_for_pathview_2, MW1a:MW1c), 1, function(x) mean(x))
KEGG_int_for_pathview_2$MW2 <- apply(select(KEGG_int_for_pathview_2, MW2a:MW2c), 1, function(x) mean(x))
KEGG_int_for_pathview_2$MW3 <- apply(select(KEGG_int_for_pathview_2, MW3a:MW3c), 1, function(x) mean(x))
KEGG_int_for_pathview_2$MW4 <- apply(select(KEGG_int_for_pathview_2, MW4a:MW4c), 1, function(x) mean(x))
KEGG_int_for_pathview_2$MW5 <- apply(select(KEGG_int_for_pathview_2, MW5a:MW5c), 1, function(x) mean(x))
KEGG_int_for_pathview_2$MW6 <- apply(select(KEGG_int_for_pathview_2, MW6a:MW6c), 1, function(x) mean(x))
KEGG_int_for_pathview_2$MW7 <- apply(select(KEGG_int_for_pathview_2, MW7a:MW7c), 1, function(x) mean(x))
KEGG_int_for_pathview_2$MW8 <- apply(select(KEGG_int_for_pathview_2, MW8a:MW8c), 1, function(x) mean(x))
KEGG_int_for_pathview_2 <- select(KEGG_int_for_pathview_2, MW1:MW8)
KEGG_int_for_pathview_norm <- data.frame(apply(KEGG_int_for_pathview_2, 1, function(x) -1+2*(x-min(x))/(max(x)-min(x))))
KEGG_int_for_pathview_matrix <- t(as.matrix(KEGG_int_for_pathview_norm))
head(KEGG_int_for_pathview_matrix)
write.csv(KEGG_int_for_pathview_matrix, "KEGG_comp.csv")
