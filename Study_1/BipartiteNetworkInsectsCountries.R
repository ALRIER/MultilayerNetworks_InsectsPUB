library(readr)
EdibleSpecies <- read_delim("Study_1/EdibleSpecies.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
variable.names(EdibleSpecies)
Country <- unique(EdibleSpecies$Country)
Edibles <- EdibleSpecies[c(5,2,4)]

library(igraph)
bn <- graph_from_data_frame(Edibles, directed = TRUE)
summary(bn)
V(bn)$name
E(bn)$Protein
V(bn)$type <- bipartite_mapping(bn)$type
V(bn)$size <- sd(E(bn)$Protein)
V(bn)$shape <- ifelse(V(bn)$type, "none", "none") 
V(bn)$labelcolor <- ifelse(V(bn)$type, "brown4", "#046A38")
E(bn)$color <- "gray"
E(bn)$width <- E(bn)$Protein/20
V(bn)$label.cex = igraph::betweenness(bn)


layout <- layout_as_bipartite(bn)
rotated_layout <- cbind(layout[, 2], -layout[, 1])


png("FS1.png", width = 7, height = 7, units = 'in', res = 300)
plot(bn, vertex.label = V(bn)$name, layout = rotated_layout, main = "",
     vertex.size = V(bn)$size,
     vertex.label.color = V(bn)$labelcolor, edge.lty = E(bn)$linetype, vertex.shape = V(bn)$shape,
     edge.arrow.size =  min(E(bn)$Protein)/15)
dev.off()
