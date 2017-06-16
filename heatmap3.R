df <- read.csv("MC_OP_identified2.csv", check.names = FALSE)
df2 <- df[-1]

df[df == 0] <- 0.5*min(df2[df2>0])

df3 <- log2(df[,2:length(df)])
df3$Area <- df$Area

cols <- colorRampPalette(c("green","red"))(8)

gplots::heatmap.2(as.matrix(df3[,-length(df3)]), Rowv = T, Colv = T, 
                  col = cols, scale = c("none"), key = F, key.title = "NULL", 
                  keysize = 1, density.info = "none", na.rm=T, cexCol = 1, 
                  cexRow = 1.2, lhei = c(1, 6), lwid = c (0.5, 4), 
                  trace = "none", margins = c(9, 15), labRow = df$Area)

dev.print(png, file = "biomass_norm_heatmap3.png", height=9, width=12, res = 600,
          units = "in")
