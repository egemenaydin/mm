library(dplyr)

data <- read.csv("Compounds_short.csv")
ppm.limit <- 2
data$mz <- data$Mass + 1.007276
fNames <- colnames(data)[3:10]

#create names
if("name" %in% colnames(data)){
        cat("names were defined\n")
} else{
        data$name <- paste("M", round(data$mz, 4), "T", round(data$RT, 4), sep = "")        
}

sup <- as.matrix(select(data, name, Mass))

DB <- read.csv("~/mm/KEGG_DB.csv")
DB <- filter(DB, KEGGid != "")
db.mass <- DB$Mass

KEGG_comp <- matrix("", ncol = 7)

for(i in 1:nrow(data)) {
        mass <- as.numeric(sup[i, 2])
        logical <- abs(((mass - db.mass)/db.mass)*10^6) < ppm.limit
        if(sum(logical)) {
                for(j in 1:length(which(logical))) {
                        KEGG_comp1 <- matrix("", nrow = 1, ncol = 7)
                        KEGG_comp1[1, 1:2] <- sup[i, 1:2]
                        KEGG_comp1[1, 3] <- db.mass[which(logical)][j]
                        KEGG_comp1[1, 4] <- as.vector(DB[which(logical), "KEGGid"])[j]
                        KEGG_comp1[1, 5] <- as.vector(DB[which(logical), "Formula"])[j]
                        KEGG_comp1[1, 6] <- as.vector(DB[which(logical), "Metabolite"])[j]
                        KEGG_comp1[1, 7] <- as.vector(DB[which(logical), "Pathway"])[j]
                        KEGG_comp <- rbind(KEGG_comp, KEGG_comp1)
                }
                
        }
        else {
                KEGG_comp1 <- matrix("", nrow = 1, ncol = 7) 
                KEGG_comp1[1, 1:2] <- sup[i, 1:2]
                KEGG_comp1[1, 3] <- "no match"
                KEGG_comp1[1, 4] <- "no match"
                KEGG_comp1[1, 5] <- "no match"
                KEGG_comp1[1, 6] <- "no match"
                KEGG_comp1[1, 7] <- "no match"
                KEGG_comp <- rbind(KEGG_comp, KEGG_comp1)
        }
}


header <- c("name", "mass", "massDB", "KEGG ID", "Formula", "Metabolite", "Pathway")
colnames(KEGG_comp) <- header
KEGG_comparison <- data.frame(KEGG_comp[-1, ])
KEGG_int <- merge(KEGG_comparison, data, by = "name")

pathview1 <- filter(KEGG_int, KEGG.ID != "no match")
pathview2 <- pathview1[, c("KEGG.ID", fNames)]
pathview22 <- pathview2[!duplicated(pathview2$KEGG.ID),]
rownames(pathview22) <- pathview22$KEGG.ID
pathview22$KEGG.ID <- NULL

for (i in 1:length(grs)) {
        pathview22[[paste(grs[[i]])]] <- apply(pathview22[,grepl(grs[[i]], colnames(pathview22))], 1, mean)
}

pathview3 <- pathview22[, fNames]
pv_norm <- data.frame(apply(pathview3, 1, function(x) -1+2*(x-min(x))/(max(x)-min(x))))
pv_matrix <- t(as.matrix(pv_norm))
head(pv_matrix)

#get csv files
write.csv(pathview1, "KEGG_comp_full.csv")
write.csv(pathview3, "KEGG_comp_short.csv")
write.csv(pv_matrix, "KEGG_comp_short_norm.csv")
