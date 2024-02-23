library(readr)
Exporters <- read_csv("Data/InsectsLeadingExporters.csv")
ExpImp <- Exporters[c(2,3,8)]

library(igraph)
bn <- graph_from_data_frame(ExpImp, directed = TRUE)
bipartite.mapping(bn)
V(bn)$type <- bipartite_mapping(bn)$type
V(bn)$labelcolor <- ifelse(V(bn)$type, "#094FFF", "#046A38")
V(bn)$shape <- "none" 
#V(bn)$labelcolor <- ifelse(V(bn)$type, "#FF671F", "#046A38")
E(bn)$color <- "gray"
E(bn)$width <- 0.3

# Set edge attributes
#E(bn)$linetype <- ifelse(E(bn)$ProgramType == "Bachelor", 1, 2)  # 1 for solid, 2 for dashed


layout <- layout_as_bipartite(bn)
rotated_layout <- cbind(-layout[, 2], layout[, 1])
set.seed(1107)
plot(bn, vertex.label = V(bn)$name, layout = rotated_layout, main = "",
     vertex.label.color = V(bn)$labelcolor, edge.lty = E(bn)$linetype, vertex.shape = "none",
     edge.arrow.size =  0.2)
