data(gse16873.d)
data(demo.paths)
#gene
i <- 1
pv.out <- pathview(gene.data = gse16873.d[ , 1], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873", kegg.native = TRUE)
list.files(pattern = "hsa04110", full.names = TRUE)
str(pv.out)
head(pv.out$plot.data.gene)
pv.out <- pathview(gene.data = gse16873.d[, 1], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873.2layer", kegg.native = T,
                   same.layer = F)
pv.out <- pathview(gene.data = gse16873.d[, 1], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873", kegg.native = F,
                   sign.pos = demo.paths$spos[i])
dim(pv.out$plot.data.gene)
head(pv.out$plot.data.gene)
pv.out <- pathview(gene.data = gse16873.d[, 1], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873.2layer", kegg.native = F,
                   sign.pos = demo.paths$spos[i], same.layer = F)
pv.out <- pathview(gene.data = gse16873.d[, 1], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873.split", kegg.native = F,
                   sign.pos = demo.paths$spos[i], split.group = T)
dim(pv.out$plot.data.gene)
head(pv.out$plot.data.gene)
pv.out <- pathview(gene.data = gse16873.d[, 1], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873.split.expanded", kegg.native = F,
                   sign.pos = demo.paths$spos[i], split.group = T, expand.node = T)
dim(pv.out$plot.data.gene)


#compound
sim.cpd.data=sim.mol.data(mol.type="cpd", nmol=3000)
data(cpd.simtypes)
i <- 3
print(demo.paths$sel.paths[i])
pv.out <- pathview(gene.data = gse16873.d[, 1], cpd.data = sim.cpd.data,
                   pathway.id = demo.paths$sel.paths[i], species = "hsa", out.suffix = "gse16873.cpd",
                   keys.align = "y", kegg.native = T, key.pos = demo.paths$kpos1[i])
head(pv.out$plot.data.cpd)
pv.out <- pathview(gene.data = gse16873.d[, 1], cpd.data = sim.cpd.data,
                   pathway.id = demo.paths$sel.paths[i], species = "hsa", out.suffix = "gse16873.cpd",
                   keys.align = "y", kegg.native = F, key.pos = demo.paths$kpos2[i],
                   sign.pos = demo.paths$spos[i], cpd.lab.offset = demo.paths$offs[i])

#multiple samples
set.seed(10)
sim.cpd.data2 = matrix(sample(sim.cpd.data, 18000,
                              replace = T), ncol = 6)
rownames(sim.cpd.data2) = names(sim.cpd.data)
colnames(sim.cpd.data2) = paste("exp", 1:6, sep = "")
head(sim.cpd.data2, 3)
pv.out <- pathview(gene.data = gse16873.d[, 1:3],
                   cpd.data = sim.cpd.data2[, 1:2], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873.cpd.3-2s", keys.align = "y",
                   kegg.native = T, match.data = F, multi.state = T, same.layer = T)
head(pv.out$plot.data.cpd)
pv.out <- pathview(gene.data = gse16873.d[, 1:3],
                   cpd.data = sim.cpd.data2[, 1:2], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873.cpd.3-2s.match",
                   keys.align = "y", kegg.native = T, match.data = T, multi.state = T,
                   same.layer = T)
pv.out <- pathview(gene.data = gse16873.d[, 1:3],
                   cpd.data = sim.cpd.data2[, 1:2], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873.cpd.3-2s", keys.align = "y",
                   kegg.native = F, match.data = F, multi.state = T, same.layer = T)
pv.out <- pathview(gene.data = gse16873.d[, 1:3],
                   cpd.data = sim.cpd.data2[, 1:2], pathway.id = demo.paths$sel.paths[i],
                   species = "hsa", out.suffix = "gse16873.cpd.3-2s", keys.align = "y",
                   kegg.native = T, match.data = F, multi.state = F, same.layer = T)
pv.out <- pathview(gene.data = gse16873.d[, 1:3],
                     cpd.data = sim.cpd.data2[, 1:2], pathway.id = demo.paths$sel.paths[i],
                     species = "hsa", out.suffix = "gse16873.cpd.3-2s.2layer",
                     keys.align = "y", kegg.native = T, match.data = F, multi.state = T,
                     same.layer = F)
