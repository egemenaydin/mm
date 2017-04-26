#aminoacids
aa_data <- bind_rows(alanine, arginine_bio, arginine_met, cysteine, glycine, histidine, lysine_bio, 
                     lysine_deg,phenylalanine_bio, phenylalanine_met, tryptophan, tyrosine)

aa_data_unique <- aa_data[!duplicated(aa_data$kegg.names),]

aa_heatmap <- dplyr::select(aa_data_unique, c(kegg.names, AT100_tp0.x:AT10_tp96.x))
write.csv(aa_heatmap, "aa_heatmap.csv")
rownames(aa_heatmap) <- aa_heatmap$kegg.names


aa_heatmap2 <- dplyr::filter(aa_heatmap, NK_tp0.x < 0 & NK_tp6.x < 0 & NK_tp24.x < 0 & NK_tp48.x < 0)

cols <- colorRampPalette(c("green","red"))(16)
gplots::heatmap.2(as.matrix(aa_heatmap2[,-1]), Rowv = T, Colv = T, 
          col = cols, scale = c("none"),
          key = F, key.title = "NULL", keysize = 1, density.info = "none", 
          na.rm=T, cexCol = 0.75, lhei = c(1, 6), lwid = c (0.25, 4), 
          trace = "none", margins = c(5, 15), labRow = aa_heatmap2$kegg.names)

dev.print(png, file = "aa_heatmap.png", height=12, width=9, res = 600, units = "in")


#lipids
lipid_data <- bind_rows(fa_bio, ufa_bio, glu, pent, GPL, GL)

lipid_data_unique <- lipid_data[!duplicated(lipid_data$kegg.names),]

lipid_heatmap <- dplyr::select(lipid_data_unique, c(kegg.names, AT100_tp0.x:AT10_tp96.x))
write.csv(lipid_heatmap, "lipid_heatmap.csv")

lipid_heatmap2 <- dplyr::filter(lipid_heatmap, NK_tp0.x < 0 & NK_tp6.x < 0 & NK_tp24.x < 0 & NK_tp48.x < 0)

cols <- colorRampPalette(c("green","red"))(16)
gplots::heatmap.2(as.matrix(lipid_heatmap2[,-1]), Rowv = T, Colv = T, 
                  col = cols, scale = c("none"),
                  key = F, key.title = "NULL", keysize = 1, density.info = "none", 
                  na.rm=T, cexCol = 0.75, lhei = c(1, 6), lwid = c (0.25, 4), 
                  trace = "none", margins = c(5, 15), labRow = lipid_heatmap2$kegg.names)

dev.print(png, file = "lipid_heatmap.png", height=12, width=9, res = 600, units = "in")
