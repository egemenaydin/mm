library(KEGGgraph)
library(KEGG.db)
library(Rgraphviz)

pID <- mget("Toluene degradation", KEGGPATHNAME2ID)

retrieveKGML(pID, organism = "ko", destfile = "toluene.xml")

mapkKGML <- system.file("toluene.xml", package= "KEGGgraph")

mapkG <- parseKGML2Graph("toluene.xml", genesOnly = FALSE)
mapkG

mapkpathway <- parseKGML("toluene.xml")
mapkpathway

mapkG2 <- KEGGpathway2Graph(mapkpathway, genesOnly = FALSE)
mapkG2

mapkNodes <- nodes(mapkG)
nodes(mapkG)[1:3]

mapkEdges <- edges(mapkG)
edges(mapkG)[1]

mapkGnodedata <- getKEGGnodeData(mapkG)
mapkGnodedata[[2]]

mapkGedgedata <- getKEGGedgeData(mapkG)
mapkGedgedata[[4]]



map <- parseKGML("toluene.xml")

reactions <- getReactions(map)

makeAttr <- function(graph, default, valNodeList) {
        tmp <- nodes(graph)
        x <- rep(default, length(tmp)); names(x) <- tmp
        
        if(!missing(valNodeList)) {
                stopifnot(is.list(valNodeList))
                allnodes <- unlist(valNodeList)
                stopifnot(all(allnodes %in% tmp))
                for(i in seq(valNodeList)) {
                        x[valNodeList[[i]]] <- names(valNodeList)[i]
                }
        }
        return(x)
}

chemicalGraph <- KEGGpathway2reactionGraph(map)
outDegrees <- sapply(edges(chemicalGraph), length)
maxout <- names(sort(outDegrees,decreasing=TRUE))[1:3]
nAttrs <- list()
maxoutlabel <- as.list(maxout); names(maxoutlabel) <- maxout
nAttrs$label <- makeAttr(chemicalGraph, "", maxoutlabel)
nAttrs$fillcolor <- makeAttr(chemicalGraph, "lightblue", list(orange=maxout))
nAttrs$width <- makeAttr(chemicalGraph,"0.8", list("1.8"=maxout))
plot(chemicalGraph, nodeAttrs=nAttrs)

