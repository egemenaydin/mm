library(pathview)
KEGG_list <- c(1:length(KEGG_comparison$KEGG.ID))
names(KEGG_list) <- KEGG_comparison$KEGG.ID
glycerph_met <- download.kegg(pathway.id = "00564", species = "ko")
pv.out_glycerph_met <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00564", species = "ko", kegg.native = T)

bile_acid <- download.kegg(pathway.id = "00120", species = "ko")
pv.out_bile_acid <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00120", species = "ko", out.suffix = "_2", kegg.native = T)

arachidonic_met <- download.kegg(pathway.id = "00590", species = "ko")
pv.out_arach_acid <- pathview(cpd.data = KEGG_list, out.suffix = "_2", pathway.id = "00590", species = "ko", kegg.native = T)

retinol_met <- download.kegg(pathway.id = "00830", species = "ko")
pv.out_retinol_met <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00830", species = "ko", kegg.native = T)

naphthalene_deg <- download.kegg(pathway.id = "00626", species = "ko")
pv.out_nap_deg <- pathview(cpd.data = KEGG_list, out.suffix = "_2", pathway.id = "00626", species = "ko", kegg.native = T)

xylene_deg <- download.kegg(pathway.id = "00622", species = "ko")
pv.out_xyl_deg <- pathview(cpd.data = KEGG_list, pathway.id = "00622", species = "ko", kegg.native = T)

benzoate_deg <- download.kegg(pathway.id = "00362", species = "ko")
pv.out_benzoate_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00362", species = "ko", kegg.native = T)

nt_deg <- download.kegg(pathway.id = "00633", species = "ko")
pv.out_nt_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00633", species = "ko", kegg.native = T)

fb_deg <- download.kegg(pathway.id = "00364", species = "ko")
pv.out_fb_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00364", species = "ko", kegg.native = T)

eb_deg <- download.kegg(pathway.id = "00642", species = "ko")
pv.out_eb_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00642", species = "ko", kegg.native = T)

PAH_deg <- download.kegg(pathway.id = "00624", species = "ko")
pv.out_PAH_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, out.suffix = "_2", pathway.id = "00624", species = "ko", kegg.native = T)

atrazine_deg <- download.kegg(pathway.id = "00791", species = "ko")
pv.out_atrazine_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, out.suffix = "_2", pathway.id = "00791", species = "ko", kegg.native = T)

DDT_deg <- download.kegg(pathway.id = "00351", species = "ko")
pv.out_DDT_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00351", species = "ko", kegg.native = T)

bisphenol_deg <- download.kegg(pathway.id = "00363", species = "ko")
pv.out_bisphenol_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00363", species = "ko", kegg.native = T)

steroind_deg <- download.kegg(pathway.id = "00984", species = "ko")
pv.out_steroin_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00984", species = "ko", kegg.native = T)

styrene_deg <- download.kegg(pathway.id = "00643", species = "ko")
pv.out_styrene_deg <- pathview(cpd.data = KEGG_int_for_pathview_matrix, pathway.id = "00643", species = "ko", kegg.native = T)
