

con <- read.csv("camelina_consumed.csv")
con <- dplyr::select(con, mz, Day0:Day40)
pro <- read.csv("camelina_produced.csv")
pro <- dplyr::select(pro, mz, Day0:Day40)
df <- rbind(con, pro)

df2 <- df[-1]

df[df == 0] <- 0.5*min(df2[df2>0])

df3 <- log2(df[,2:length(df)])
df3$mz <- df$mz

cols <- colorRampPalette(c("green","red"))(16)

gplots::heatmap.2(as.matrix(df3[,-length(df3)]), Rowv = T, Colv = F, 
                  col = cols, scale = c("none"), key = F, key.title = "NULL", 
                  keysize = 1, density.info = "none", na.rm=T, cexCol = 1, 
                  cexRow = 1.2, lhei = c(1, 6), lwid = c (0.5, 4), 
                  trace = "none", margins = c(9, 15), labRow = df$mz)

dev.print(png, file = "biomass_norm_heatmap3.png", height=9, width=12, res = 600,
          units = "in")
